use memchr::memmem::Finder;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::ops::Deref;
use std::slice::Iter;
use std::sync::Arc;
use std::vec::IntoIter;

/// A unit of a LIKE pattern.
#[derive(Debug, Clone)]
pub enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

#[derive(Debug)]
pub struct Tokens<'a> {
    tokens: Vec<Token<'a>>,
}

pub type TokensIntoIter<'a> = IntoIter<Token<'a>>;
impl<'a> IntoIterator for Tokens<'a> {
    type Item = Token<'a>;
    type IntoIter = TokensIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

type TokensIter<'a> = Iter<'a, Token<'a>>;

impl<'a> IntoIterator for &'a Tokens<'a> {
    type Item = &'a Token<'a>;
    type IntoIter = TokensIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.iter()
    }
}

/// Lexes a LIKE pattern into tokens. Never fails because all strings are valid patterns.
pub fn lex(input: &str) -> Tokens {
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        let (t, rest) = lex_one(s);
        tokens.push(t);
        s = rest;
    }

    Tokens { tokens }
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
    SkipToLiteral(Cow<'a, str>),
    /// Consume any character, at least the given number of times.
    AtLeast(usize),
    /// Consume exactly the given number of characters.
    Exactly(usize),
    /// Consume the end of the string.
    End,
    /// Consume the entire string.
    All,
    /// Consume the entire string if it starts with the given prefix.
    StartsWith(Cow<'a, str>),
    /// Consume the entire string if it ends with the given suffix.
    EndsWith(Cow<'a, str>),
    /// Consume the entire string if it contains the given substring.
    Contains(Cow<'a, str>),
    /// Consume the entire string if it equals the given string.
    Equals(Cow<'a, str>),
}

/// A sequence of matchers.
/// This can be thought of as the "Intermediate Representation" (IR) of the matching engine.
/// We run optimizations on it to reduce the search space during matching.
#[derive(Debug, Clone)]
struct Matchers<'a> {
    matchers: Vec<Matcher<'a>>,
}

impl<'a> Matchers<'a> {
    fn from_tokens(tokens: Tokens<'a>) -> Matchers<'a> {
        let mut v = Vec::new();

        v.extend(tokens.into_iter().map(|token| match token {
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
                (Matcher::SkipToLiteral(s), _) if s.is_empty() => {
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
                (Matcher::SkipToLiteral(ref a), Matcher::Literal(ref b)) => {
                    let a = a.clone();
                    let b = b.clone();
                    let c = Cow::Owned(a.to_string() + b.deref());
                    v.push(Matcher::SkipToLiteral(c));
                    i += 1;
                    changed = true;
                }

                // Combine "at least" and literal to make "skip to literal".
                // This allows us to use highly-optimized substring search algorithms.
                (Matcher::AtLeast(a), Matcher::Literal(s) | Matcher::SkipToLiteral(s)) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::SkipToLiteral(s.clone()));
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
                (Matcher::AtLeast(a), Matcher::All | Matcher::End) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::All);
                    i += 1;
                    changed = true;
                }

                (Matcher::AtLeast(a), Matcher::Equals(s)) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::AtLeast(a), Matcher::Contains(s)) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::AtLeast(a), Matcher::StartsWith(s)) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::AtLeast(a), Matcher::EndsWith(s)) => {
                    if *a > 0 {
                        v.push(Matcher::Exactly(*a));
                    }
                    v.push(Matcher::EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::Literal(s), Matcher::End) => {
                    v.push(Matcher::Equals(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::SkipToLiteral(s), Matcher::End) => {
                    v.push(Matcher::EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::Literal(s), Matcher::All) => {
                    v.push(Matcher::StartsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Matcher::SkipToLiteral(s), Matcher::All) => {
                    v.push(Matcher::Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                // No optimizations found here, just move on.
                _ => {
                    v.push(a.clone());
                }
            }

            i += 1;
        }

        if i < self.matchers.len() {
            v.push(self.matchers[i].clone());
        }

        let ret = Matchers { matchers: v };
        if changed {
            Ok(ret)
        } else {
            Err(ret)
        }
    }
}

#[derive(Debug, Clone)]
enum NFATransition {
    /// Transition is allowed if we can consume the given prefix.
    Prefix(String),
    /// Transition is allowed if we can skip to and consume the given substring.
    // The arc is because multiple branches will point to the same finder.
    SkipToSubString(Arc<Finder<'static>>),
    /// Transition is allowed if there are n characters to consume.
    Skip(usize),
    /// Transition is allowed if we have consumed the entire string.
    End,
    /// Transition is always allowed, consuming the entire strign.
    All,
    /// Transition is allowed if the string starts with the given prefix.
    AllIfStartsWith(String),
    /// Transition is allowed if the string ends with the given suffix.
    AllIfEndsWith(String),
    /// Transition is allowed if the string contains the given substring.
    AllIfContains(Finder<'static>),
    /// Transition is allowed if the string equals the given string.
    AllIfEquals(String),
}

type State = usize;

#[derive(Debug, Clone)]
struct TransitionTable {
    transitions: Vec<Vec<(State, NFATransition)>>,
}

impl TransitionTable {
    fn new() -> TransitionTable {
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

    fn add(&mut self, from: State, to: State, transition: NFATransition) {
        self.transitions[from].push((to, transition));
    }

    fn transitions(&self, state: State) -> impl Iterator<Item = (State, &NFATransition)> {
        self.transitions[state]
            .iter()
            .map(|&(state, ref transition)| (state, transition))
    }
}

#[derive(Debug, Clone)]
struct NFA {
    transitions: TransitionTable,
}

impl NFA {
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

                // We implement this, but after optimization it would never be used.
                // There are always more efficient ways to represent the same pattern.
                Matcher::AtLeast(_) => {
                    unreachable!("AtLeast matchers should be optimized away.");
                }

                Matcher::Literal(s) => {
                    let next_state = transitions.next_state();

                    // Allow transitioning to the next state if the string starts with the given prefix.
                    transitions.add(
                        prev_state,
                        next_state,
                        NFATransition::Prefix(s.into_owned()),
                    );

                    prev_state = next_state;
                }

                Matcher::SkipToLiteral(cow) => {
                    let next_state = transitions.next_state();
                    let finder = Finder::new(cow.as_bytes()).into_owned();
                    let finder = Arc::new(finder);

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

                Matcher::All => {
                    transitions.add(prev_state, end_state, NFATransition::All);
                }

                Matcher::StartsWith(s) => {
                    transitions.add(
                        prev_state,
                        end_state,
                        NFATransition::AllIfStartsWith(s.into_owned()),
                    );
                }

                Matcher::EndsWith(s) => {
                    transitions.add(
                        prev_state,
                        end_state,
                        NFATransition::AllIfEndsWith(s.into_owned()),
                    );
                }

                Matcher::Contains(s) => {
                    transitions.add(
                        prev_state,
                        end_state,
                        NFATransition::AllIfContains(Finder::new(s.as_bytes()).into_owned()),
                    );
                }

                Matcher::Equals(s) => {
                    transitions.add(
                        prev_state,
                        end_state,
                        NFATransition::AllIfEquals(s.into_owned()),
                    );
                }
            }
        }

        NFA { transitions }
    }

    pub fn matches(&self, s: &str) -> bool {
        // Holds the execution state of the NFA.
        let mut state_to_rem = VecDeque::new();
        state_to_rem.push_back((self.transitions.start_state(), s));

        while let Some((state, rem)) = state_to_rem.pop_front() {
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
                    NFATransition::Skip(n) => {
                        let (num_chars, char_bytes) = rem
                            .chars()
                            .take(*n)
                            .fold((0, 0), |(num_chars, char_bytes), c| {
                                (num_chars + 1, char_bytes + c.len_utf8())
                            });

                        if num_chars < *n {
                            // We can't skip this many characters.
                            continue;
                        }

                        state_to_rem.push_back((next_state, &rem[char_bytes..]));
                    }

                    NFATransition::Prefix(prefix) => {
                        if rem.starts_with(prefix.deref()) {
                            state_to_rem.push_back((next_state, &rem[prefix.len()..]));
                        }
                    }

                    NFATransition::SkipToSubString(finder) => {
                        if let Some(pos) = finder.find(rem.as_bytes()) {
                            let consumed_bytes = pos + finder.needle().len();
                            state_to_rem.push_back((next_state, &rem[consumed_bytes..]));
                        }
                    }

                    NFATransition::End => {
                        if rem.is_empty() {
                            return true;
                        }
                    }

                    NFATransition::All => {
                        return true;
                    }

                    NFATransition::AllIfStartsWith(prefix) => {
                        if rem.starts_with(prefix.deref()) {
                            return true;
                        }
                    }

                    NFATransition::AllIfEndsWith(suffix) => {
                        if rem.ends_with(suffix.deref()) {
                            return true;
                        }
                    }

                    NFATransition::AllIfContains(finder) => {
                        if finder.find(rem.as_bytes()).is_some() {
                            return true;
                        }
                    }

                    NFATransition::AllIfEquals(s) => {
                        if rem == s {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }
}

#[derive(Debug, Clone)]
pub struct LikeMatcher {
    nfa: NFA,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let matchers = Matchers::from_tokens(tokens).optimize();
        let nfa = NFA::from_matchers(matchers);
        LikeMatcher { nfa }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.nfa.matches(input)
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
        assert!(LikeMatcher::new("_______________________").matches("aaaaaaaaaaaaaaaaaaaaaaa"));
        assert!(LikeMatcher::new("h_llo").matches("hello"));
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
    fn test_greediness() {
        assert!(LikeMatcher::new("'%'").matches("' 'hello' world'"));
        assert!(LikeMatcher::new("a '%' b c").matches("a 'd' b c 'd' b c"));
        assert!(LikeMatcher::new("'%'%'").matches("'a'a'a'a'a'a'a'a'a'"));
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 14,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_matching_never_fails(pattern in ".*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string containing a special character".
        fn test_matching_never_fails_special(pattern in r".*[%_\\].*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string containing 2 consecutive special characters".
        fn test_matching_never_fails_consecutive_special(pattern in r".*[%_\\]{2}.*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The pattern can be read as "A string not containing any special characters".
        fn test_literals_always_match(input in r"[^%_\\]*") {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
