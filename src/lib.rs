use bit_set::BitSet;
use itertools::Itertools;
use memchr::memmem::Finder;
use rustc_hash::FxHashMap;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
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
        use Matcher::*;
        let mut v = Vec::new();
        let mut changed = false;

        let mut i = 0;
        while i + 1 < self.matchers.len() {
            let a = &self.matchers[i];
            let b = &self.matchers[i + 1];

            match (a, b) {
                // Remove empty literals.
                (Literal(s), _) if s.is_empty() => {
                    i += 1;
                    changed = true;
                }

                // Remove empty character counters.
                (Exactly(0), _) => {
                    i += 1;
                    changed = true;
                }

                // Remove empty "skip to literal".
                (SkipToLiteral(s), _) if s.is_empty() => {
                    i += 1;
                    changed = true;
                }

                // Combine adjacent literals.
                // Escaping can cause literals to be split during lexing.
                (Literal(ref a), Literal(ref b)) => {
                    let a = a.clone();
                    let b = b.clone();
                    let c = Cow::Owned(a.to_string() + b.deref());
                    v.push(Literal(c));
                    i += 1;
                    changed = true;
                }

                // Combine adjacent "skip to literal" and literal.
                (SkipToLiteral(ref a), Literal(ref b)) => {
                    let a = a.clone();
                    let b = b.clone();
                    let c = Cow::Owned(a.to_string() + b.deref());
                    v.push(SkipToLiteral(c));
                    i += 1;
                    changed = true;
                }

                // Combine "at least" and literal to make "skip to literal".
                // This allows us to use highly-optimized substring search algorithms.
                (AtLeast(a), Literal(s) | SkipToLiteral(s)) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(SkipToLiteral(s.clone()));
                    i += 1;
                    changed = true;
                }

                // Combine character counters like "___" into a single matcher.
                (Exactly(a), Exactly(b)) => {
                    v.push(Exactly(a + b));
                    i += 1;
                    changed = true;
                }

                // Combine any combination of "at least" and "exactly" counters.
                (AtLeast(a), Exactly(b)) | (AtLeast(a), AtLeast(b)) | (Exactly(b), AtLeast(a)) => {
                    v.push(AtLeast(a + b));
                    i += 1;
                    changed = true;
                }

                // Optimizes for "match any ending" patterns like "hello%".
                (AtLeast(a), All | End) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(All);
                    i += 1;
                    changed = true;
                }

                (AtLeast(a), Equals(s)) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (AtLeast(a), Contains(s)) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                (AtLeast(a), StartsWith(s)) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                (AtLeast(a), EndsWith(s)) => {
                    if *a > 0 {
                        v.push(Exactly(*a));
                    }
                    v.push(EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Literal(s), End) => {
                    v.push(Equals(s.clone()));
                    i += 1;
                    changed = true;
                }

                (SkipToLiteral(s), End) => {
                    v.push(EndsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Literal(s), All) => {
                    v.push(StartsWith(s.clone()));
                    i += 1;
                    changed = true;
                }

                (SkipToLiteral(s), All) => {
                    v.push(Contains(s.clone()));
                    i += 1;
                    changed = true;
                }

                (Literal(a), StartsWith(b)) => {
                    v.push(StartsWith(Cow::Owned(a.to_string() + b.deref())));
                    i += 1;
                    changed = true;
                }

                (Literal(a), Equals(b)) => {
                    v.push(Equals(Cow::Owned(a.to_string() + b.deref())));
                    i += 1;
                    changed = true;
                }

                (SkipToLiteral(a), StartsWith(b)) => {
                    v.push(Contains(Cow::Owned(a.to_string() + b.deref())));
                    i += 1;
                    changed = true;
                }

                (SkipToLiteral(a), Equals(b)) => {
                    v.push(EndsWith(Cow::Owned(a.to_string() + b.deref())));
                    i += 1;
                    changed = true;
                }

                (Literal(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), AtLeast(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), Exactly(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), AtLeast(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), Exactly(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Literal(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), End) => {
                    v.push(a.clone());
                }

                (Exactly(_), All) => {
                    v.push(a.clone());
                }

                (Exactly(_), StartsWith(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Equals(_)) => {
                    v.push(a.clone());
                }

                (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_), _) => {
                    unreachable!("{:?} should always be the last matcher", a);
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
enum Transition {
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

impl Hash for Transition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Transition::Prefix(s) => {
                state.write_u8(0);
                s.hash(state);
            }

            Transition::SkipToSubString(finder) => {
                state.write_u8(1);
                finder.needle().hash(state);
            }

            Transition::Skip(n) => {
                state.write_u8(2);
                n.hash(state);
            }

            Transition::End => {
                state.write_u8(3);
            }

            Transition::All => {
                state.write_u8(4);
            }

            Transition::AllIfStartsWith(s) => {
                state.write_u8(5);
                s.hash(state);
            }

            Transition::AllIfEndsWith(s) => {
                state.write_u8(6);
                s.hash(state);
            }

            Transition::AllIfContains(finder) => {
                state.write_u8(7);
                finder.needle().hash(state);
            }

            Transition::AllIfEquals(s) => {
                state.write_u8(8);
                s.hash(state);
            }
        }
    }
}

impl PartialEq<Transition> for Transition {
    fn eq(&self, other: &Transition) -> bool {
        match (self, other) {
            (Transition::Prefix(a), Transition::Prefix(b)) => a == b,
            (Transition::SkipToSubString(a), Transition::SkipToSubString(b)) => {
                a.needle() == b.needle()
            }
            (Transition::Skip(a), Transition::Skip(b)) => a == b,
            (Transition::End, Transition::End) => true,
            (Transition::All, Transition::All) => true,
            (Transition::AllIfStartsWith(a), Transition::AllIfStartsWith(b)) => a == b,
            (Transition::AllIfEndsWith(a), Transition::AllIfEndsWith(b)) => a == b,
            (Transition::AllIfContains(a), Transition::AllIfContains(b)) => {
                a.needle() == b.needle()
            }
            (Transition::AllIfEquals(a), Transition::AllIfEquals(b)) => a == b,
            _ => false,
        }
    }
}

// This defines the precedence of transitions.
impl PartialOrd<Transition> for Transition {
    fn partial_cmp(&self, other: &Transition) -> Option<Ordering> {
        match (self, other) {
            (Transition::All, _) => Some(Ordering::Less),
            (_, Transition::All) => Some(Ordering::Greater),

            (Transition::AllIfStartsWith(_), _) => Some(Ordering::Less),
            (_, Transition::AllIfStartsWith(_)) => Some(Ordering::Greater),

            (Transition::AllIfEndsWith(_), _) => Some(Ordering::Less),
            (_, Transition::AllIfEndsWith(_)) => Some(Ordering::Greater),

            (Transition::AllIfContains(_), _) => Some(Ordering::Less),
            (_, Transition::AllIfContains(_)) => Some(Ordering::Greater),

            (Transition::AllIfEquals(_), _) => Some(Ordering::Less),
            (_, Transition::AllIfEquals(_)) => Some(Ordering::Greater),

            (Transition::End, _) => Some(Ordering::Less),
            (_, Transition::End) => Some(Ordering::Greater),

            (Transition::Skip(_), _) => Some(Ordering::Less),
            (_, Transition::Skip(_)) => Some(Ordering::Greater),

            (Transition::SkipToSubString(_), _) => Some(Ordering::Less),
            (_, Transition::SkipToSubString(_)) => Some(Ordering::Greater),

            (Transition::Prefix(_), _) => Some(Ordering::Less),
        }
    }
}

impl Ord for Transition {
    fn cmp(&self, other: &Transition) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for Transition {}

type State = usize;

#[derive(Debug, Clone)]
struct TransitionTable {
    transitions: Vec<Vec<(State, Transition)>>,
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

    fn next_state(&mut self) -> State {
        self.transitions.push(Vec::new());
        self.transitions.len() - 1
    }

    fn add(&mut self, from: State, to: State, transition: Transition) {
        self.transitions[from].push((to, transition));
    }

    fn transitions(&self, state: State) -> impl Iterator<Item = (State, &Transition)> {
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

        for matcher in matchers.matchers {
            match matcher {
                Matcher::Exactly(n) => {
                    let next_state = transitions.next_state();
                    transitions.add(prev_state, next_state, Transition::Skip(n));
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
                    transitions.add(prev_state, next_state, Transition::Prefix(s.into_owned()));

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
                        Transition::SkipToSubString(finder.clone()),
                    );

                    // Allow trying to skip again by transitioning to the same state.
                    transitions.add(next_state, next_state, Transition::SkipToSubString(finder));

                    prev_state = next_state;
                }

                Matcher::End => {
                    transitions.add(prev_state, prev_state, Transition::End);
                }

                Matcher::All => {
                    transitions.add(prev_state, prev_state, Transition::All);
                }

                Matcher::StartsWith(s) => {
                    transitions.add(
                        prev_state,
                        prev_state,
                        Transition::AllIfStartsWith(s.into_owned()),
                    );
                }

                Matcher::EndsWith(s) => {
                    transitions.add(
                        prev_state,
                        prev_state,
                        Transition::AllIfEndsWith(s.into_owned()),
                    );
                }

                Matcher::Contains(s) => {
                    transitions.add(
                        prev_state,
                        prev_state,
                        Transition::AllIfContains(Finder::new(s.as_bytes()).into_owned()),
                    );
                }

                Matcher::Equals(s) => {
                    transitions.add(
                        prev_state,
                        prev_state,
                        Transition::AllIfEquals(s.into_owned()),
                    );
                }
            }
        }

        NFA { transitions }
    }
}

#[derive(Debug, Clone)]
struct DFA {
    transitions: Vec<Vec<(Transition, State)>>,
}

type DFABuilderState = BitSet;

impl DFA {
    fn from_nfa(nfa: NFA) -> Self {
        // Holds the transitions of the DFA.
        // We will later translate this to an optimized form for fast matching.
        let mut transition_map = FxHashMap::default();

        // This is the start state of the DFA.
        let dfa_start_state = DFABuilderState::from_iter([nfa.transitions.start_state()]);

        // This machinery helps us traverse the DFA, ensuring we don't visit the same state twice.
        let mut q = VecDeque::from([dfa_start_state.clone()]);
        let mut num_states_visited = 0;
        let mut visit_order = FxHashMap::default();

        // Traverse the DFA using BFS.
        while let Some(curr_dfa_state) = q.pop_front() {
            // Check whether we have visited this state before.
            if visit_order.contains_key(&curr_dfa_state) {
                continue;
            }
            visit_order.insert(curr_dfa_state.clone(), num_states_visited);
            num_states_visited += 1;

            // Build up a map from transitions to the next (NFA) state sets.
            let mut local_transitions = FxHashMap::default();
            for curr_nfa_state in curr_dfa_state.iter() {
                for (next_nfa_state, transition) in nfa.transitions.transitions(curr_nfa_state) {
                    local_transitions
                        .entry(transition)
                        .or_insert_with(|| BitSet::new())
                        .insert(next_nfa_state);
                }
            }

            // Continue the traversal by adding next states to the queue.
            for next_dfa_state in local_transitions.values() {
                q.push_back(next_dfa_state.clone());
            }

            // Record the outcome for the current DFA state.
            transition_map.insert(
                curr_dfa_state.clone(),
                local_transitions.into_iter().collect::<Vec<_>>(),
            );
        }

        // We have converted the NFA to a DFA! Now we need to optimize it.
        // Make sure the DFA start state gets assigned to 0.
        let order_to_state = visit_order
            .iter()
            .map(|(state, order)| (*order, state.clone()))
            .sorted()
            .collect::<FxHashMap<_, _>>();
        let mut transitions = Vec::new();
        for i in 0..num_states_visited {
            let dfa_state = order_to_state[&i].clone();
            let mut t = Vec::new();
            for (transition, next_state) in transition_map
                .remove(&dfa_state)
                .unwrap()
                .into_iter()
                .sorted()
            {
                let next_order = visit_order[&next_state];
                t.push((transition.clone(), next_order));
            }
            transitions.push(t);
        }
        DFA { transitions }
    }

    fn start_state(&self) -> State {
        0
    }

    fn matches(&self, s: &str) -> bool {
        let mut state = self.start_state();
        let mut rem = s;
        loop {
            for (transition, next_state) in &self.transitions[state] {
                match transition {
                    Transition::End => {
                        return rem.is_empty();
                    }

                    Transition::All => {
                        return true;
                    }

                    Transition::AllIfStartsWith(prefix) => {
                        return rem.starts_with(prefix.deref());
                    }

                    Transition::AllIfEndsWith(suffix) => {
                        return rem.ends_with(suffix.deref());
                    }

                    Transition::AllIfContains(finder) => {
                        return finder.find(rem.as_bytes()).is_some();
                    }

                    Transition::AllIfEquals(s) => {
                        return rem == s;
                    }

                    Transition::Skip(n) => {
                        let (num_chars, char_bytes) = rem
                            .chars()
                            .take(*n)
                            .fold((0, 0), |(num_chars, char_bytes), c| {
                                (num_chars + 1, char_bytes + c.len_utf8())
                            });

                        if num_chars < *n {
                            // We can't skip this many characters.
                            return false;
                        }

                        state = *next_state;
                        rem = &rem[char_bytes..];
                    }

                    Transition::Prefix(prefix) => {
                        if !rem.starts_with(prefix.deref()) {
                            return false;
                        }

                        state = *next_state;
                        rem = &rem[prefix.len()..];
                    }

                    Transition::SkipToSubString(finder) => {
                        if let Some(pos) = finder.find(rem.as_bytes()) {
                            let consumed_bytes = pos + finder.needle().len();
                            state = *next_state;
                            rem = &rem[consumed_bytes..];
                        } else {
                            return false;
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LikeMatcher {
    dfa: DFA,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let matchers = Matchers::from_tokens(tokens).optimize();
        let nfa = NFA::from_matchers(matchers);
        let dfa = DFA::from_nfa(nfa);
        LikeMatcher { dfa }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.dfa.matches(input)
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
        assert!(LikeMatcher::new("'%'%%'%'").matches("'a'a'a'a'a'a'a'a'a'"));
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
