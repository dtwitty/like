use memchr::memmem::Finder;
use std::borrow::Cow;
use std::ops::Deref;

/// A unit of a LIKE pattern.
#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

/// Lexes a LIKE pattern into tokens. Never fails because all strings are valid patterns.
fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        let (t, rest) = lex_one(s);
        tokens.push(t);
        s = rest;
    }

    tokens
}

/// Lexes a single token from the input. Never fails because all strings are valid patterns.
fn lex_one(i: &str) -> (Token, &str) {
    match i {
        s if s.starts_with('%') => (Token::Any, &s[1..]),
        s if s.starts_with('_') => (Token::Single, &s[1..]),
        s if s.starts_with("\\\\") => (Token::Literal("\\"), &s[2..]),
        s if s.starts_with("\\%") => (Token::Literal("%"), &s[2..]),
        s if s.starts_with("\\_") => (Token::Literal("_"), &s[2..]),
        s if s.starts_with('\\') => (Token::Literal("\\"), &s[1..]),
        _ => {
            let pos = memchr::memchr3(b'%', b'_', b'\\', i.as_bytes());
            let pos = pos.unwrap_or(i.len());
            (Token::Literal(&i[..pos]), &i[pos..])
        }
    }
}

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Debug, Clone)]
enum Matcher<'a> {
    /// Consume a literal string.
    Literal(Cow<'a, str>),
    /// Consume any number of characters and match a literal.
    UntilLiteral(Cow<'a, str>),
    /// Matches any character, at least the given number of times.
    AtLeast(usize),
    /// Matches exactly the given number of characters.
    Exactly(usize),
    /// Matches the end of the string.
    End,
    /// Finish the match immediately. This used for end optimization like "hello%".
    Finish,
}

/// A sequence of matchers.
/// This can be thought of as the "Intermediate Representation" (IR) of the matching engine.
/// We run optimizations on it to reduce the search space during matching.
#[derive(Debug, Clone)]
struct Matchers<'a> {
    matchers: Vec<Matcher<'a>>,
}

impl<'a> Matchers<'a> {
    fn from_tokens(tokens: Vec<Token<'a>>) -> Matchers<'a> {
        let mut v = Vec::new();

        v.extend(tokens.iter().map(|token| match token {
            Token::Literal(s) => Matcher::Literal(Cow::Borrowed(s)),
            Token::Any => Matcher::AtLeast(0),
            Token::Single => Matcher::Exactly(1),
        }));

        v.push(Matcher::End);
        Matchers { matchers: v }
    }

    fn optimize(self) -> Self {
        let mut curr = self;
        loop {
            match curr.optimize_one() {
                Ok(ir) => curr = ir,
                Err(ir) => return ir,
            }
        }
    }

    fn optimize_one(self) -> Result<Self, Self> {
        let mut v = Vec::new();
        let mut changed = false;

        let mut i = 0;
        while i + 1 < self.matchers.len() {
            let a = &self.matchers[i];
            let b = &self.matchers[i + 1];

            // This match block is essentially the list of optimizations.
            match (a, b) {
                // Remove empty literals.
                (Matcher::Literal(s), _) if s.is_empty() => {
                    i += 1;
                    changed = true;
                }

                // Remove empty character counters.
                (Matcher::Exactly(0), _) => {
                    i += 1;
                    changed = true;
                }

                // Remove empty "skip to literal".
                (Matcher::UntilLiteral(s), _) if s.is_empty() => {
                    i += 1;
                    changed = true;
                }

                // Combine adjacent literals.
                // Escaping can cause literals to be split during lexing.
                (Matcher::Literal(ref a), Matcher::Literal(ref b)) => {
                    let a = a.clone();
                    let b = b.clone();
                    let c = Cow::Owned(a.to_string() + b.deref());
                    v.push(Matcher::Literal(c));
                    i += 1;
                    changed = true;
                }

                // Combine adjacent "skip to literal" and literal.
                (Matcher::UntilLiteral(ref a), Matcher::Literal(ref b)) => {
                    let a = a.clone();
                    let b = b.clone();
                    let c = Cow::Owned(a.to_string() + b.deref());
                    v.push(Matcher::UntilLiteral(c));
                    i += 1;
                    changed = true;
                }

                // Combine "at least" and literal to make "skip to literal".
                // This allows us to use highly-optimized substring search algorithms.
                (Matcher::AtLeast(a), Matcher::Literal(s)) => {
                    if *a > 0 {
                        v.push(Matcher::AtLeast(*a));
                    }
                    v.push(Matcher::UntilLiteral(s.clone()));
                    i += 1;
                    changed = true;
                }

                // Combine character counters like "___" into a single matcher.
                (Matcher::Exactly(a), Matcher::Exactly(b)) => {
                    v.push(Matcher::Exactly(a + b));
                    i += 1;
                    changed = true;
                }

                // Combine any combination of "at least" and "exactly" counters.
                (Matcher::AtLeast(a), Matcher::Exactly(b))
                | (Matcher::AtLeast(a), Matcher::AtLeast(b))
                | (Matcher::Exactly(b), Matcher::AtLeast(a)) => {
                    v.push(Matcher::AtLeast(a + b));
                    i += 1;
                    changed = true;
                }

                // Optimizes for "match any ending" patterns like "hello%".
                (Matcher::AtLeast(a), Matcher::Finish | Matcher::End) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::Finish);
                    i += 1;
                    changed = true;
                }

                _ => {
                    v.push(a.clone());
                }
            }

            i += 1;
        }

        if i < self.matchers.len() {
            v.push(self.matchers[i].clone());
        }

        if changed {
            Ok(Matchers { matchers: v })
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone)]
enum NFATransition<'a> {
    /// Transition is allowed if we can consume the given prefix.
    Prefix(Cow<'a, str>),
    /// Transition is allowed if we can skip to and consume the given substring.
    SkipToSubString(Finder<'a>),
    /// Transition is allowed if there are n characters to consume.
    Skip(usize),
    /// Transition is always allowed, consuming no characters.
    Empty,
    /// Transition is allowed if we have consumed the entire string.
    End,
    /// If encountered (with any destination state), the NFA can immediately declare a match.
    SucceedImmediately,
}

impl<'a> NFATransition<'a> {
    fn into_owned(self) -> NFATransition<'static> {
        match self {
            NFATransition::Prefix(prefix) => NFATransition::Prefix(Cow::Owned(prefix.to_string())),
            NFATransition::SkipToSubString(finder) => {
                NFATransition::SkipToSubString(finder.into_owned())
            }
            NFATransition::Skip(n) => NFATransition::Skip(n),
            NFATransition::Empty => NFATransition::Empty,
            NFATransition::SucceedImmediately => NFATransition::SucceedImmediately,
            NFATransition::End => NFATransition::End,
        }
    }
}

type State = usize;

#[derive(Debug, Clone)]
struct TransitionTable<'a> {
    transitions: Vec<Vec<(State, NFATransition<'a>)>>,
}

impl<'a> TransitionTable<'a> {
    fn new() -> TransitionTable<'a> {
        TransitionTable {
            // Allocate start and end states,
            transitions: vec![Vec::new(), Vec::with_capacity(0)],
        }
    }

    fn start_state(&self) -> State {
        0
    }

    fn end_state(&self) -> State {
        1
    }

    fn next_state(&mut self) -> State {
        self.transitions.push(Vec::new());
        self.transitions.len() - 1
    }

    fn add(&mut self, from: State, to: State, transition: NFATransition<'a>) {
        self.transitions[from].push((to, transition));
    }

    fn transitions(&self, state: State) -> impl Iterator<Item = (State, &NFATransition<'a>)> {
        self.transitions[state]
            .iter()
            .map(|&(state, ref transition)| (state, transition))
    }

    fn into_owned(self) -> TransitionTable<'static> {
        let transitions = self
            .transitions
            .into_iter()
            .map(|transitions| {
                transitions
                    .into_iter()
                    .map(|(state, transition)| (state, transition.into_owned()))
                    .collect()
            })
            .collect();
        TransitionTable { transitions }
    }
}

#[derive(Debug, Clone)]
struct NFA<'a> {
    transitions: TransitionTable<'a>,
}

impl<'a> NFA<'a> {
    pub fn from_matchers(matchers: Matchers) -> NFA {
        let mut transitions = TransitionTable::new();
        let mut prev_state = transitions.start_state();
        let end_state = transitions.end_state();

        for matcher in matchers.matchers {
            match matcher {
                Matcher::Exactly(n) => {
                    let next_state = transitions.next_state();
                    transitions.add(prev_state, next_state, NFATransition::Skip(n));
                    prev_state = next_state;
                }

                Matcher::AtLeast(n) => {
                    let a = transitions.next_state();
                    let b = transitions.next_state();

                    // Allow transitioning to `a` by consuming n characters.
                    transitions.add(prev_state, a, NFATransition::Skip(n));

                    // Allow `a` to transition to itself by consuming a single character.
                    transitions.add(a, a, NFATransition::Skip(1));

                    // Allow `a` to transition to `b` t any time.
                    transitions.add(a, b, NFATransition::Empty);

                    prev_state = b;
                }

                Matcher::Literal(s) => {
                    let next_state = transitions.next_state();

                    // Allow transitioning to the next state if the string starts with the given prefix.
                    transitions.add(prev_state, next_state, NFATransition::Prefix(s));

                    prev_state = next_state;
                }

                Matcher::UntilLiteral(s) => {
                    let next_state = transitions.next_state();
                    let finder = Finder::new(s.deref()).into_owned();

                    // Allow transitioning if we can skip to the given literal.
                    transitions.add(
                        prev_state,
                        next_state,
                        NFATransition::SkipToSubString(finder.clone()),
                    );

                    // Allow trying to skip again by transitioning to the same state.
                    transitions.add(
                        next_state,
                        next_state,
                        NFATransition::SkipToSubString(finder),
                    );

                    prev_state = next_state;
                }

                Matcher::End => {
                    transitions.add(prev_state, end_state, NFATransition::End);
                }

                Matcher::Finish => {
                    transitions.add(prev_state, end_state, NFATransition::SucceedImmediately);
                }
            }
        }

        NFA { transitions }
    }

    pub fn matches(&self, s: &str) -> bool {
        // Holds the execution state of the NFA.
        let mut state_to_rem = vec![(self.transitions.start_state(), s)];

        loop {
            let mut next_state_to_rem = Vec::new();

            for (state, rem) in state_to_rem {
                if state == self.transitions.end_state() {
                    if rem.is_empty() {
                        // We found a match!
                        return true;
                    }

                    // This path didn't work because we finished the NFA before finishing the string.
                    continue;
                }

                for (next_state, transition) in self.transitions.transitions(state) {
                    match transition {
                        NFATransition::Empty => {
                            // Transition to the next state without consuming any characters.
                            next_state_to_rem.push((next_state, rem));
                        }

                        NFATransition::Skip(n) => {
                            let (num_chars, char_bytes) =
                                rem.chars()
                                    .take(*n)
                                    .fold((0, 0), |(num_chars, char_bytes), c| {
                                        (num_chars + 1, char_bytes + c.len_utf8())
                                    });

                            if num_chars < *n {
                                // We can't skip this many characters.
                                continue;
                            }

                            next_state_to_rem.push((next_state, &rem[char_bytes..]));
                        }

                        NFATransition::Prefix(prefix) => {
                            if rem.starts_with(prefix.deref()) {
                                next_state_to_rem.push((next_state, &rem[prefix.len()..]));
                            }
                        }

                        NFATransition::SkipToSubString(finder) => {
                            if let Some(pos) = finder.find(rem.as_bytes()) {
                                let consumed_bytes = pos + finder.needle().len();
                                next_state_to_rem.push((next_state, &rem[consumed_bytes..]));
                            }
                        }

                        NFATransition::SucceedImmediately => {
                            return true;
                        }

                        NFATransition::End => {
                            if rem.is_empty() {
                                next_state_to_rem.push((next_state, rem));
                            }
                        }
                    }
                }
            }

            if next_state_to_rem.is_empty() {
                return false;
            }

            state_to_rem = next_state_to_rem;
        }
    }

    pub fn into_owned(self) -> NFA<'static> {
        let transitions = self.transitions.into_owned();
        NFA { transitions }
    }
}

#[derive(Debug, Clone)]
pub struct LikeMatcher<'a> {
    nfa: NFA<'a>,
}

impl<'a> LikeMatcher<'a> {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let matchers = Matchers::from_tokens(tokens).optimize();
        let nfa = NFA::from_matchers(matchers);
        LikeMatcher { nfa }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.nfa.matches(input)
    }

    pub fn into_owned(self) -> LikeMatcher<'static> {
        let nfa = self.nfa.into_owned();
        LikeMatcher { nfa }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test_empty() {
        assert!(LikeMatcher::new("").matches(""));
        assert!(!LikeMatcher::new("").matches("world"));
    }

    #[test]
    fn test_literal() {
        assert!(LikeMatcher::new("world").matches("world"));
        assert!(!LikeMatcher::new("hello").matches("world"));
    }

    #[test]
    fn test_starts_with() {
        assert!(LikeMatcher::new("hello%").matches("hello world"));
        assert!(!LikeMatcher::new("hello%").matches("world"));
    }

    #[test]
    fn test_ends_with() {
        assert!(LikeMatcher::new("%world").matches("hello world"));
        assert!(!LikeMatcher::new("%world").matches("hello"));
    }

    #[test]
    fn test_contains() {
        assert!(LikeMatcher::new("%world%").matches("hello world"));
        assert!(LikeMatcher::new("%hello%").matches("hello world"));
        assert!(LikeMatcher::new("%llo wo%").matches("hello world"));
        assert!(!LikeMatcher::new("%world%").matches("hello"));
        assert!(LikeMatcher::new("%🔥%").matches("hello 🔥"));
        assert!(LikeMatcher::new("%🔥%").matches("🔥hello"));
    }

    #[test]
    fn test_single() {
        assert!(!LikeMatcher::new("_").matches(""));
        assert!(LikeMatcher::new("_").matches("w"));
        assert!(!LikeMatcher::new("_").matches("he"));
        assert!(LikeMatcher::new("h_llo").matches("hello"));
        assert!(!LikeMatcher::new("h_llo").matches("world"));
        assert!(!LikeMatcher::new("h_llo").matches("world"));
        assert!(LikeMatcher::new("h_llo").matches("h🔥llo"));
    }

    #[test]
    fn test_any() {
        assert!(LikeMatcher::new("%").matches("hello world"));
        assert!(LikeMatcher::new("%%").matches("hello world"));
        assert!(LikeMatcher::new("%").matches(""));
        assert!(LikeMatcher::new("%%").matches(""));
    }

    #[test]
    fn test_any_single() {
        assert!(!LikeMatcher::new("h_%o").matches("ho"));
        assert!(!LikeMatcher::new("%_").matches(""));
        assert!(!LikeMatcher::new("_%").matches(""));
        assert!(LikeMatcher::new("%_").matches("hello world"));
        assert!(LikeMatcher::new("_%").matches("hello world"));
        assert!(LikeMatcher::new("%_").matches("h"));
        assert!(LikeMatcher::new("_%").matches("h"));
        assert!(LikeMatcher::new("h_%o").matches("hello"));
        assert!(LikeMatcher::new("h%_o").matches("hello"));
        assert!(LikeMatcher::new("h_%o").matches("hlo"));
        assert!(LikeMatcher::new("h%_o").matches("hlo"));
        assert!(!LikeMatcher::new("h%_o").matches("ho"));
        assert!(!LikeMatcher::new("h_%o").matches("world"));
        assert!(!LikeMatcher::new("h%_o").matches("world"));
    }

    #[test]
    fn test_escape() {
        assert!(LikeMatcher::new(r"hello\%").matches("hello%"));
        assert!(LikeMatcher::new(r"hello\_").matches("hello_"));
        assert!(!LikeMatcher::new(r"hello\%").matches("hello"));
        assert!(!LikeMatcher::new(r"hello\_").matches("hello"));
        assert!(LikeMatcher::new(r"hel\\o%").matches("hel\\o"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hel\\p"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hel\\"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hl\\o"));
        assert!(LikeMatcher::new(r"h\%o").matches("h%o"));
    }

    #[test]
    fn test_greedy() {
        // This is a regression test for a bug where the '%' would match as little as possible.
        // The '%' should match as much as possible.
        assert!(LikeMatcher::new("a '%' b c").matches("a 'd' b c 'd' b c"));
        assert!(LikeMatcher::new("'%'").matches("' 'hello' world'"));
    }

    #[test]
    fn test_owning() {
        let s = "%hello%".to_string();
        // This matcher depends on the string `s`.
        let matcher = LikeMatcher::new(&s);
        // `into_owned` should make the matcher independent of `s`, copying where necessary.
        let matcher = matcher.into_owned();
        // Prove that the matcher is independent of `s`. If not, this won't compile.
        drop(s);
        assert!(matcher.matches("hello world"));
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 14,
            // Use small strings to explore more interesting behavior.
            max_default_size_range: 16,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_matching_never_sfails(pattern in ".*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string containing a special character".
        fn test_matching_never_panics_special(pattern in r".*[%_\\].*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string containing 2 consecutive special characters".
        fn test_matching_never_panics_consecutive_special(pattern in r".*[%_\\]{2}.*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string of `_` and `%` containing 3 `_` characters".
        fn test_only_special(pattern in r"(%*_%*){3}", input in ".{3,10}") {
            let matcher = LikeMatcher::new(&pattern);
            assert!(matcher.matches(&input));
        }

        #[test]
        // The pattern can be read as "A string not containing any special characters".
        fn test_literals_always_match(input in r"[^%_\\]*") {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
