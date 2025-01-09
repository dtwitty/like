#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

fn lex(input: &str) -> Vec<Token> {
    println!("Input: {:?}", input);
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        let (t, rest) = lex_one(s);
        tokens.push(t);
        s = rest;
    }

    tokens
}

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

#[derive(Debug, Clone)]
enum NFATransition {
    /// Transition is allowed if the string starts with the given prefix.
    Prefix(String),
    /// Transition is allowed if there is at least one character left in the string.
    Single,
    /// Transition is always allowed.
    Empty,
}

type StateId = usize;

#[derive(Debug, Clone)]
struct TransitionTable<T> {
    transitions: Vec<Vec<(StateId, T)>>,
}

impl<T> TransitionTable<T> {
    fn new() -> TransitionTable<T> {
        TransitionTable {
            transitions: vec![Vec::new()],
        }
    }

    fn start_state(&self) -> StateId {
        0
    }

    fn next_state(&mut self) -> StateId {
        self.transitions.push(Vec::new());
        self.transitions.len() - 1
    }

    fn add(&mut self, from: StateId, to: StateId, transition: T) {
        self.transitions[from].push((to, transition));
    }

    fn transitions(&self, state_id: StateId) -> impl Iterator<Item = (StateId, &T)> {
        self.transitions[state_id]
            .iter()
            .map(|&(state, ref transition)| (state, transition))
    }
}

#[derive(Debug, Clone)]
struct NFA {
    transitions: TransitionTable<NFATransition>,
    end_state: StateId,
}

impl NFA {
    pub fn from_tokens(tokens: Vec<Token>) -> NFA {
        println!("Tokens: {:?}", tokens);
        let mut prev_state_id = 0;
        let mut transitions = TransitionTable::new();

        for token in tokens {
            match token {
                Token::Single => {
                    let next_state_id = transitions.next_state();
                    transitions.add(prev_state_id, next_state_id, NFATransition::Single);
                    prev_state_id = next_state_id;
                }

                Token::Any => {
                    let a = transitions.next_state();
                    let b = transitions.next_state();

                    // Allow skipping over this token.
                    transitions.add(prev_state_id, b, NFATransition::Empty);

                    // Allow transitioning to `a` by consuming a single character.
                    transitions.add(prev_state_id, a, NFATransition::Single);

                    // Allow `a` to transition to itself by consuming a single character.
                    transitions.add(a, a, NFATransition::Single);

                    // Allow `a` to transition to `b` t any time.
                    transitions.add(a, b, NFATransition::Empty);

                    prev_state_id = b;
                }

                Token::Literal(s) => {
                    let next_state_id = transitions.next_state();

                    // Allow transitioning to the next state if the string starts with the given prefix.
                    transitions.add(
                        prev_state_id,
                        next_state_id,
                        NFATransition::Prefix(s.to_string()),
                    );

                    prev_state_id = next_state_id;
                }
            }
        }

        NFA {
            transitions,
            end_state: prev_state_id,
        }
    }

    pub fn matches(&self, s: &str) -> bool {
        println!("Transitions: {:?}", self.transitions);
        // Holds the execution state of the NFA.
        let mut state_to_rem = vec![(self.transitions.start_state(), s)];

        loop {
            let mut next_state_to_rem = Vec::new();

            for (state, rem) in state_to_rem {
                if state == self.end_state {
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

                        NFATransition::Single => {
                            if rem.is_empty() {
                                // This is a dead end. We needed at least one character.
                                continue;
                            }

                            // Split off a single character.
                            let char_bytes = rem.chars().next().unwrap().len_utf8();
                            next_state_to_rem.push((next_state, &rem[char_bytes..]));
                        }

                        NFATransition::Prefix(prefix) => {
                            if rem.starts_with(prefix) {
                                next_state_to_rem.push((next_state, &rem[prefix.len()..]));
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
}

#[derive(Debug, Clone)]
pub struct LikeMatcher {
    nfa: NFA,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let nfa = NFA::from_tokens(tokens);
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
        assert!(LikeMatcher::new("_").matches("w"));
        assert!(!LikeMatcher::new("_").matches(""));
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
        assert!(!LikeMatcher::new("h_%o").matches("ho"));
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

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 3,
            // Use small strings to explore more interesting behavior.
            max_default_size_range: 16,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_matching_never_fails(pattern in ".*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        fn test_matching_never_fails_special(pattern in r".*[%_\\].*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        fn test_matching_never_fails_consecutive_special(pattern in r".*[%_\\]{2}.*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        fn test_literals_always_match(input in r"[^%_\\]*") {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
