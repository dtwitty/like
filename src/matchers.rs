use crate::matchers::MedialMatcher::SkipToLiteral;
use crate::patterns::{Pattern, Patterns};
use memchr::memmem::Finder;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub(crate) enum TerminalMatcher {
    /// Consume the entire string.
    All,
    /// Consume the end of the string.
    End,
    /// Consume the entire string if it starts with the given prefix.
    StartsWith(String),
    StartsWithChar(char),
    /// Consume the entire string if it ends with the given suffix.
    EndsWith(String),
    EndsWithChar(char),
    /// Consume the entire string if it contains the given substring.
    Contains(Finder<'static>),
    /// Consume the entire string if it equals the given string.
    Equals(String),
    EqualsChar(char),
    /// Consume the entire string if it has the given length.
    Len(NonZeroUsize),
}

impl TerminalMatcher {
    pub fn matches(&self, s: &str) -> bool {
        use TerminalMatcher::*;
        match self {
            All => true,
            End => s.is_empty(),
            StartsWith(prefix) => s.starts_with(prefix),
            StartsWithChar(c) => s.chars().next().map_or(false, |d| d == *c),
            EndsWith(suffix) => s.ends_with(suffix),
            EndsWithChar(c) => s.chars().next_back().map_or(false, |d| d == *c),
            Contains(finder) => finder.find(s.as_bytes()).is_some(),
            Equals(s2) => s == s2,
            EqualsChar(c) => {
                let mut chars = s.chars();
                if let Some(first) = chars.next() {
                    // Needs to match and there must be no other character.
                    first == *c && chars.next().is_none()
                } else {
                    // No first character.
                    false
                }
            }

            Len(n) => {
                let mut chars = s.chars();
                chars.nth(n.get() - 1).is_some() && chars.next().is_none()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum MedialMatcher {
    /// Consume a literal string.
    Literal(String),
    LiteralChar(char),
    /// Consume any number of characters and match a literal.
    SkipToLiteral(Finder<'static>),
    /// Consume exactly the given number of characters.
    Exactly(NonZeroUsize),
}

impl MedialMatcher {
    pub fn matches<'a>(&self, s: &'a str) -> Option<&'a str> {
        use MedialMatcher::*;
        match self {
            Literal(lit) => {
                if s.starts_with(lit) {
                    Some(&s[lit.len()..])
                } else {
                    None
                }
            }

            LiteralChar(c) => {
                let mut chars = s.chars();
                let d = chars.next()?;
                if d == *c {
                    Some(chars.as_str())
                } else {
                    None
                }
            }

            SkipToLiteral(finder) => finder
                .find(s.as_bytes())
                .map(|pos| unsafe { s.get_unchecked(pos + finder.needle().len()..) }),

            Exactly(n) if n.get() == 1 => {
                let mut chars = s.chars();
                chars.next().map(|_| chars.as_str())
            }

            Exactly(n) => {
                let mut chars = s.chars();
                chars.nth(n.get() - 1)?;
                Some(chars.as_str())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Matcher {
    Terminal(TerminalMatcher),
    Medial(MedialMatcher),
}

impl Matcher {
    fn from_pattern(p: &Pattern) -> Self {
        use Pattern::*;

        match p {
            All => Matcher::Terminal(TerminalMatcher::All),

            End => Matcher::Terminal(TerminalMatcher::End),

            StartsWith(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::StartsWithChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::StartsWith(s.to_string()))
                }
            }

            EndsWith(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::EndsWithChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::EndsWith(s.to_string()))
                }
            }

            Contains(s) => {
                let s = s.to_string();
                Matcher::Terminal(TerminalMatcher::Contains(
                    Finder::new(s.as_bytes()).into_owned(),
                ))
            }

            Equals(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::EqualsChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::Equals(s.to_string()))
                }
            }

            Len(n) => Matcher::Terminal(TerminalMatcher::Len(NonZeroUsize::try_from(*n).unwrap())),

            Literal(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::Medial(MedialMatcher::LiteralChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Medial(MedialMatcher::Literal(s.to_string()))
                }
            }

            SkipToLiteral(s) => {
                let s = s.to_string();
                Matcher::Medial(MedialMatcher::SkipToLiteral(
                    Finder::new(s.as_bytes()).into_owned(),
                ))
            }

            Exactly(n) => {
                Matcher::Medial(MedialMatcher::Exactly(NonZeroUsize::try_from(*n).unwrap()))
            }

            AtLeast(_) => panic!("AtLeast should have been optimized away"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Matchers {
    matchers: Vec<Matcher>,
    contains_singles: bool,
}

impl Deref for Matchers {
    type Target = [Matcher];

    fn deref(&self) -> &Self::Target {
        &self.matchers
    }
}

impl Matchers {
    pub fn from_patterns(patterns: &Patterns) -> Self {
        use Matcher::*;
        use MedialMatcher::*;
        use TerminalMatcher::*;

        let matchers: Vec<Matcher> = patterns.iter().map(Matcher::from_pattern).collect();
        let contains_singles = matchers
            .iter()
            .any(|m| matches!(m, Medial(Exactly(_))) || matches!(m, Terminal(Len(_))));
        Matchers {
            matchers,
            contains_singles,
        }
    }

    pub fn matches(&self, s: &str) -> bool {
        use Matcher::*;

        let mut remaining_input = s;
        let mut remaining_matchers = &self.matchers[..];
        let mut last_wildcard = None;

        while !remaining_matchers.is_empty() {
            let m = &remaining_matchers[0];
            match m {
                Terminal(tm) if tm.matches(remaining_input) => {
                    return true;
                }

                Medial(mm) if mm.matches(remaining_input).is_some() => {
                    // We expect the compiler to optimize the repeated sub-expression.
                    let rem = mm.matches(remaining_input).unwrap();

                    if let SkipToLiteral(finder) = mm {
                        // If the pattern doesn't contain any single chars, we never need to backtrack.
                        if self.contains_singles {
                            // We may need to backtrack to this position.
                            // The next thing we should try is to take one character from the start of the match.
                            let needle_len = finder.needle().len();
                            let rem_len = rem.len();
                            let matched_at = remaining_input.len() - rem_len - needle_len;
                            let t = remaining_input.get(matched_at..).unwrap();
                            let next_try_input = strip_char(t);
                            last_wildcard = Some((remaining_matchers, next_try_input));
                        }
                    }

                    remaining_input = rem;
                    remaining_matchers = &remaining_matchers[1..];
                }

                _ if !remaining_input.is_empty() => {
                    // Match has failed, see if we can backtrack to the last saved wildcard.
                    if let Some((matchers, next_try_input)) = last_wildcard {
                        if matchers.len() == remaining_matchers.len()
                            && next_try_input.len() == remaining_input.len()
                        {
                            // This would cause an infinite loop!
                            return false;
                        }

                        remaining_matchers = matchers;
                        remaining_input = next_try_input;
                    } else {
                        return false;
                    }
                }

                _ => {
                    return false;
                }
            }
        }

        remaining_input.is_empty()
    }
}

impl Display for Matchers {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn strip_char(s: &str) -> &str {
    let mut chars = s.chars();
    chars.next();
    chars.as_str()
}
