use crate::automata::{State, StateTransition, TerminalTransition};
use crate::automata::{StateTransition::*, TerminalTransition::*};
use crate::matchers::{Matcher, Matchers};
use memchr::memmem::Finder;
use std::sync::Arc;

#[derive(Debug, Clone)]
struct NFATransitions {
    terminal_transition: Option<TerminalTransition>,
    state_transitions: Vec<(StateTransition, State)>,
}

impl NFATransitions {
    fn new() -> NFATransitions {
        NFATransitions {
            terminal_transition: None,
            state_transitions: Vec::with_capacity(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NFA {
    transitions: Vec<NFATransitions>,
}

impl NFA {
    pub fn new() -> NFA {
        NFA {
            transitions: vec![NFATransitions::new(), NFATransitions::new()],
        }
    }

    pub fn start_state(&self) -> State {
        State(0)
    }

    pub fn get_next_state(&mut self) -> State {
        self.transitions.push(NFATransitions::new());
        State(self.transitions.len() - 1)
    }

    pub fn add_state_transition(&mut self, from: State, to: State, transition: StateTransition) {
        self.transitions[from.0]
            .state_transitions
            .push((transition, to));
    }

    pub fn set_terminal_transition(&mut self, from: State, transition: TerminalTransition) {
        let o = &mut self.transitions[from.0];
        assert!(o.terminal_transition.is_none());
        o.terminal_transition = Some(transition);
    }

    pub fn state_transitions(
        &self,
        state: State,
    ) -> impl Iterator<Item = &(StateTransition, State)> {
        self.transitions[state.0].state_transitions.iter()
    }

    pub fn terminal_transition(&self, state: State) -> Option<&TerminalTransition> {
        self.transitions[state.0].terminal_transition.as_ref()
    }

    #[cfg(test)]
    pub fn states(&self) -> impl Iterator<Item = State> {
        (0..self.transitions.len()).map(State)
    }

    pub fn from_matchers(matchers: Matchers) -> NFA {
        let mut nfa = NFA::new();
        let mut prev_state = nfa.start_state();

        for matcher in matchers {
            match matcher {
                Matcher::Exactly(n) => {
                    let next_state = nfa.get_next_state();
                    nfa.add_state_transition(prev_state, next_state, Skip(n));
                    prev_state = next_state;
                }

                // We implement this, but after optimization it would never be used.
                // There are always more efficient ways to represent the same pattern.
                Matcher::AtLeast(_) => {
                    unreachable!("AtLeast matchers should be optimized away.");
                }

                Matcher::Literal(s) => {
                    let next_state = nfa.get_next_state();

                    // Allow transitioning to the next State if the string starts with the given prefix.
                    nfa.add_state_transition(prev_state, next_state, Prefix(s.into_owned()));

                    prev_state = next_state;
                }

                Matcher::SkipToLiteral(cow) => {
                    let next_state = nfa.get_next_state();
                    let finder = Finder::new(cow.as_bytes()).into_owned();
                    let finder = Arc::new(finder);

                    // Allow transitioning if we can skip to the given literal.
                    nfa.add_state_transition(
                        prev_state,
                        next_state,
                        SkipToSubString(finder.clone()),
                    );

                    // Allow trying to skip again by transitioning to the same state.
                    nfa.add_state_transition(next_state, next_state, SkipToSubString(finder));

                    prev_state = next_state;
                }

                Matcher::End => {
                    nfa.set_terminal_transition(prev_state, End);
                }

                Matcher::All => {
                    nfa.set_terminal_transition(prev_state, All);
                }

                Matcher::StartsWith(s) => {
                    nfa.set_terminal_transition(prev_state, AllIfStartsWith(s.into_owned()));
                }

                Matcher::EndsWith(s) => {
                    nfa.set_terminal_transition(prev_state, AllIfEndsWith(s.into_owned()));
                }

                Matcher::Contains(s) => {
                    nfa.set_terminal_transition(
                        prev_state,
                        AllIfContains(Finder::new(s.as_bytes()).into_owned()),
                    );
                }

                Matcher::Equals(s) => {
                    nfa.set_terminal_transition(prev_state, AllIfEquals(s.into_owned()));
                }

                Matcher::Len(n) => {
                    nfa.set_terminal_transition(prev_state, AllIfLen(n));
                }
            }
        }

        nfa
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::lex;
    use proptest::prelude::ProptestConfig;
    use proptest::proptest;

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 14,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_invariants(pattern in ".*") {
            let tokens = lex(&pattern);
            let matchers = Matchers::from_tokens(tokens).optimize();
            let nfa = NFA::from_matchers(matchers);

            for state in nfa.states() {
                // Check invariants for individual transitions.
                for (transition, _) in nfa.state_transitions(state) {
                    match transition {
                        Skip(n) => {
                            assert!(*n > 0);
                        }

                        SkipToSubString(s) => {
                            assert!(!s.needle().is_empty());
                        }

                        Prefix(s) => {
                            assert!(!s.is_empty());
                        }
                    }
                }
            }
        }
    }
}
