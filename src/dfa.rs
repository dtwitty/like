use crate::automata::{State, StateTransition, TerminalTransition};
use crate::nfa::NFA;
use bit_set::BitSet;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct DFABuilderTransitions {
    terminal_transition: Option<TerminalTransition>,
    state_transitions: FxHashMap<StateTransition, BitSet>,
}

#[derive(Debug, Clone)]
struct DFATransitions {
    terminal_transition: Option<TerminalTransition>,
    state_transitions: Vec<(StateTransition, State)>,
}

impl DFATransitions {
    fn new() -> Self {
        DFATransitions {
            terminal_transition: None,
            state_transitions: Vec::with_capacity(0),
        }
    }

    fn set_terminal_transition(&mut self, transition: TerminalTransition) {
        assert!(self.terminal_transition.is_none());
        self.terminal_transition = Some(transition);
    }

    fn add_state_transition(&mut self, transition: StateTransition, state: State) {
        self.state_transitions.push((transition, state));
    }
}

#[derive(Debug, Clone)]
pub struct DFA {
    transitions: Vec<DFATransitions>,
}

impl DFA {
    pub fn from_nfa(nfa: NFA) -> Self {
        // Holds the transitions of the DFA.
        // We will later translate this to an optimized form for fast matching.
        let mut transition_map = FxHashMap::default();

        // This is the start state of the DFA.
        let dfa_start_state = BitSet::from_iter([nfa.start_state().0]);

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
            let mut terminal_transition = None;
            let mut state_transitions = FxHashMap::default();
            for curr_nfa_state in curr_dfa_state.iter().map(State::new) {

                if let Some(transition) = nfa.terminal_transition(curr_nfa_state) {
                    assert!(terminal_transition.is_none());
                    terminal_transition = Some(transition.clone());
                }

                for (transition, next_nfa_state) in nfa.state_transitions(curr_nfa_state) {
                    state_transitions
                        .entry(transition.clone())
                        .or_insert_with(|| BitSet::new())
                        .insert(next_nfa_state.0);
                }
            }

            // Continue the traversal by adding next states to the queue.
            for next_dfa_state in state_transitions.values() {
                q.push_back(next_dfa_state.clone());
            }

            // Record the outcome for the current DFA state.
            let local_transitions = DFABuilderTransitions {
                terminal_transition,
                state_transitions,
            };

            transition_map.insert(curr_dfa_state.clone(), local_transitions);
        }

        // We have converted the NFA to a DFA! Now we need to optimize it.
        // Associate each state in the DFA with a number, which is the order we visited them in.
        let order_to_state = visit_order
            .iter()
            .map(|(state, order)| (*order, state.clone()))
            .collect::<FxHashMap<_, _>>();

        let mut transitions = Vec::new();
        for i in 0..num_states_visited {
            let dfa_state = order_to_state[&i].clone();
            let builder_transitions = transition_map.remove(&dfa_state).unwrap();
            let mut optimized_transitions = DFATransitions::new();

            for (transition, next_dfa_state) in builder_transitions.state_transitions {
                let next_order = visit_order[&next_dfa_state];
                optimized_transitions.add_state_transition(transition, State(next_order));
            }

            if let Some(transition) = builder_transitions.terminal_transition {
                optimized_transitions.set_terminal_transition(transition);
            }

            transitions.push(optimized_transitions);
        }

        DFA { transitions }
    }

    fn start_state(&self) -> State {
        State(0)
    }

    pub fn matches(&self, s: &str) -> bool {
        let mut state = self.start_state();
        let mut rem = s;
        'outer: loop {
            let transitions = &self.transitions[state.0];

            if let Some(transition) = &transitions.terminal_transition {
                return transition.try_transition(rem);
            }

            for (transition, next_state) in &transitions.state_transitions {
                if let Some(next) = transition.try_transition(rem) {
                    state = *next_state;
                    rem = next;
                    continue 'outer;
                }
            }

            return false;
        }
    }

    pub fn states(&self) -> impl Iterator<Item = State> {
        (0..self.transitions.len()).map(State)
    }

    pub fn terminal_transition(&self, s: State) -> Option<&TerminalTransition> {
        self.transitions[s.0].terminal_transition.as_ref()
    }

    #[cfg(test)]
    fn state_transitions(&self, s: State) -> impl Iterator<Item = &(StateTransition, State)> {
        self.transitions[s.0].state_transitions.iter()
    }
}

/*
#[cfg(test)]
mod tests {
    use crate::automata::StateTransition::*;
    use crate::dfa::DFA;
    use crate::matchers::Matchers;
    use crate::nfa::NFA;
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
            let dfa = DFA::from_nfa(nfa);

            for state in dfa.states() {
                // Ensure that all state transitions are mutually exclusive.
                let transitions = dfa.state_transitions(*state).collect();
                for i in 0..transitions.len() {
                    for j in 0..transitions.len() {
                        if i == j {
                            continue;
                        }

                        let (t1, _) = transitions[i];
                        let (t2, _) = transitions[j];


                    }
                }
            }

        }
    }
}
*/