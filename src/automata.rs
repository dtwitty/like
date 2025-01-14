use memchr::memmem::Finder;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::discriminant;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct State(pub usize);

impl State {
    pub fn new(n: usize) -> Self {
        State(n)
    }
}

#[derive(Clone)]
pub enum StateTransition {
    /// Transition is allowed if we can consume the given prefix.
    Prefix(String),
    /// Transition is allowed if we can skip to and consume the given substring.
    // The arc is because multiple branches will point to the same finder.
    SkipToSubString(Arc<Finder<'static>>),
    /// Transition is allowed if there are n characters to consume.
    Skip(usize),
}

impl Debug for StateTransition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StateTransition::Prefix(s) => write!(f, "Prefix(\"{}\")", s),
            StateTransition::SkipToSubString(finder) => write!(
                f,
                "SkipToSubString(\"{}\")",
                std::str::from_utf8(finder.needle()).unwrap()
            ),
            StateTransition::Skip(n) => write!(f, "Skip({})", n),
        }
    }
}

impl StateTransition {
    pub fn try_transition<'a>(&self, s: &'a str) -> Option<&'a str> {
        match self {
            StateTransition::Prefix(prefix) => {
                if s.starts_with(prefix) {
                    Some(&s[prefix.len()..])
                } else {
                    None
                }
            }

            StateTransition::SkipToSubString(finder) => {
                let pos = finder.find(s.as_bytes())?;
                let consumed_bytes = pos + finder.needle().len();
                Some(&s[consumed_bytes..])
            }

            StateTransition::Skip(n) => {
                let (num_chars, char_bytes) = s
                    .chars()
                    .take(*n)
                    .fold((0, 0), |(num_chars, char_bytes), c| {
                        (num_chars + 1, char_bytes + c.len_utf8())
                    });

                if num_chars < *n {
                    // We can't skip this many characters.
                    None
                } else {
                    Some(&s[char_bytes..])
                }
            }
        }
    }
}

impl Hash for StateTransition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StateTransition::Prefix(s) => {
                state.write_u8(0);
                s.hash(state);
            }

            StateTransition::SkipToSubString(finder) => {
                state.write_u8(1);
                finder.needle().hash(state);
            }

            StateTransition::Skip(n) => {
                state.write_u8(2);
                n.hash(state);
            }
        }
    }
}

impl PartialEq<StateTransition> for StateTransition {
    fn eq(&self, other: &StateTransition) -> bool {
        match (self, other) {
            (StateTransition::Prefix(a), StateTransition::Prefix(b)) => a == b,
            (StateTransition::SkipToSubString(a), StateTransition::SkipToSubString(b)) => {
                a.needle() == b.needle()
            }
            (StateTransition::Skip(a), StateTransition::Skip(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for StateTransition {}

#[derive(Clone)]
pub enum TerminalTransition {
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
    /// Transition is allowed if the string has the given length.
    AllIfLen(usize),
}

impl Debug for TerminalTransition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalTransition::End => write!(f, "End"),
            TerminalTransition::All => write!(f, "All"),
            TerminalTransition::AllIfStartsWith(s) => write!(f, "AllIfStartsWith(\"{}\")", s),
            TerminalTransition::AllIfEndsWith(s) => write!(f, "AllIfEndsWith(\"{}\")", s),
            TerminalTransition::AllIfContains(finder) => {
                write!(
                    f,
                    "AllIfContains({})",
                    std::str::from_utf8(finder.needle()).unwrap()
                )
            }
            TerminalTransition::AllIfEquals(s) => write!(f, "AllIfEquals(\"{}\")", s),
            TerminalTransition::AllIfLen(n) => write!(f, "AllIfLen({})", n),
        }
    }
}

impl TerminalTransition {
    pub fn try_transition(&self, s: &str) -> bool {
        match self {
            TerminalTransition::End => s.is_empty(),
            TerminalTransition::All => true,
            TerminalTransition::AllIfStartsWith(prefix) => s.starts_with(prefix),
            TerminalTransition::AllIfEndsWith(suffix) => s.ends_with(suffix),
            TerminalTransition::AllIfContains(finder) => finder.find(s.as_bytes()).is_some(),
            TerminalTransition::AllIfEquals(s2) => s == s2,
            TerminalTransition::AllIfLen(n) => s.chars().map(|c| c.len_utf8()).sum::<usize>() == *n,
        }
    }
}

impl Hash for TerminalTransition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TerminalTransition::End => {
                discriminant(self).hash(state);
            }

            TerminalTransition::All => {
                discriminant(self).hash(state);
            }

            TerminalTransition::AllIfStartsWith(s) => {
                discriminant(self).hash(state);
                s.hash(state);
            }

            TerminalTransition::AllIfEndsWith(s) => {
                discriminant(self).hash(state);
                s.hash(state);
            }

            TerminalTransition::AllIfContains(finder) => {
                discriminant(self).hash(state);
                finder.needle().hash(state);
            }

            TerminalTransition::AllIfEquals(s) => {
                discriminant(self).hash(state);
                s.hash(state);
            }

            TerminalTransition::AllIfLen(n) => {
                discriminant(self).hash(state);
                n.hash(state);
            }
        }
    }
}

impl PartialEq<TerminalTransition> for TerminalTransition {
    fn eq(&self, other: &TerminalTransition) -> bool {
        match (self, other) {
            (TerminalTransition::End, TerminalTransition::End) => true,
            (TerminalTransition::All, TerminalTransition::All) => true,
            (TerminalTransition::AllIfStartsWith(a), TerminalTransition::AllIfStartsWith(b)) => {
                a == b
            }
            (TerminalTransition::AllIfEndsWith(a), TerminalTransition::AllIfEndsWith(b)) => a == b,
            (TerminalTransition::AllIfContains(a), TerminalTransition::AllIfContains(b)) => {
                a.needle() == b.needle()
            }
            (TerminalTransition::AllIfEquals(a), TerminalTransition::AllIfEquals(b)) => a == b,
            (TerminalTransition::AllIfLen(a), TerminalTransition::AllIfLen(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for TerminalTransition {}
