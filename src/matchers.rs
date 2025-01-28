use crate::patterns::{Pattern, Patterns};
use memchr::memmem::Finder;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub(crate) enum Matcher {
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
    /// Consume a literal string.
    Literal(String),
    LiteralChar(char),
    EndLiteral(String),
    EndLiteralChar(char),
    /// Consume any number of characters and match a literal.
    SkipToLiteral(Finder<'static>),
    /// Consume exactly the given number of characters.
    Exactly(NonZeroUsize),
    EndExactly(NonZeroUsize),
}

impl Matcher {
    fn from_pattern(p: &Pattern) -> Self {
        use Pattern::*;

        match p {
            All => Matcher::All,

            End => Matcher::End,

            StartsWith(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::StartsWithChar(s.chars().next().unwrap())
                } else {
                    Matcher::StartsWith(s.to_string())
                }
            }

            EndsWith(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::EndsWithChar(s.chars().next().unwrap())
                } else {
                    Matcher::EndsWith(s.to_string())
                }
            }

            Contains(s) => {
                let s = s.to_string();
                Matcher::Contains(Finder::new(s.as_bytes()).into_owned())
            }

            Equals(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::EqualsChar(s.chars().next().unwrap())
                } else {
                    Matcher::Equals(s.to_string())
                }
            }

            Len(n) => Matcher::Len(NonZeroUsize::try_from(*n).unwrap()),

            Literal(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::LiteralChar(s.chars().next().unwrap())
                } else {
                    Matcher::Literal(s.to_string())
                }
            }

            EndLiteral(s) => {
                let s = s.to_string();
                if s.chars().count() == 1 {
                    Matcher::EndLiteralChar(s.chars().next().unwrap())
                } else {
                    Matcher::EndLiteral(s.to_string())
                }
            }

            SkipToLiteral(s) => {
                let s = s.to_string();
                Matcher::SkipToLiteral(Finder::new(s.as_bytes()).into_owned())
            }

            Exactly(n) => Matcher::Exactly(NonZeroUsize::try_from(*n).unwrap()),

            EndExactly(n) => Matcher::EndExactly(NonZeroUsize::try_from(*n).unwrap()),

            AtLeast(_) => panic!("AtLeast should have been optimized away"),

            Noop => panic!("Noop should have been optimized away"),
        }
    }

    pub fn try_match<'a>(&self, s: &'a str) -> Option<&'a str> {
        use Matcher::*;
        match self {
            All => Some(""),

            End => s.is_empty().then_some(""),

            StartsWith(prefix) => s.starts_with(prefix).then_some(""),

            StartsWithChar(c) => s.chars().next().filter(|d| d == c).and(Some("")),

            EndsWith(suffix) => s.ends_with(suffix).then_some(""),

            EndsWithChar(c) => s.chars().next_back().filter(|d| d == c).and(Some("")),

            Contains(finder) => finder.find(s.as_bytes()).and(Some("")),

            Equals(s2) => (s == s2).then_some(""),

            EqualsChar(c) => {
                let mut chars = s.chars();
                let d = chars.next()?;
                if d == *c && chars.next().is_none() {
                    Some("")
                } else {
                    None
                }
            }

            Len(n) if n.get() == 1 => {
                let mut chars = s.chars();
                chars.next()?;
                if chars.next().is_none() {
                    Some("")
                } else {
                    None
                }
            }

            Len(n) => {
                let mut chars = s.chars();
                if chars.nth(n.get() - 1).is_some() && chars.next().is_none() {
                    Some("")
                } else {
                    None
                }
            }

            Literal(lit) => s.strip_prefix(lit),

            LiteralChar(c) => {
                let mut chars = s.chars();
                let d = chars.next()?;
                if d == *c {
                    Some(chars.as_str())
                } else {
                    None
                }
            }

            EndLiteral(lit) => s.strip_suffix(lit),

            EndLiteralChar(c) => {
                let mut chars = s.chars();
                let d = chars.next_back()?;
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

            EndExactly(n) if n.get() == 1 => {
                let mut chars = s.chars();
                chars.next_back().map(|_| chars.as_str())
            }

            EndExactly(n) => {
                let mut chars = s.chars();
                chars.nth_back(n.get() - 1)?;
                Some(chars.as_str())
            }
        }
    }

    pub fn enable_backtracking(&self) -> bool {
        use Matcher::*;
        match self {
            Exactly(_) | Len(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Matchers {
    matchers: Vec<Matcher>,
}

impl Deref for Matchers {
    type Target = [Matcher];

    fn deref(&self) -> &Self::Target {
        &self.matchers
    }
}

impl Matchers {
    pub fn from_patterns(patterns: &Patterns) -> Self {
        let matchers = patterns.iter().map(Matcher::from_pattern).collect();
        Matchers { matchers }
    }

    pub fn matches(&self, s: &str) -> bool {
        use Matcher::*;

        let mut remaining_input = s;
        let mut remaining_matchers = &self.matchers[..];
        let mut last_wildcard = None;
        let mut should_backtrack = false;

        while !remaining_matchers.is_empty() {
            let m = &remaining_matchers[0];
            if m.enable_backtracking() {
                should_backtrack = true;
            }

            if let Some(rem) = m.try_match(remaining_input) {
                if let SkipToLiteral(finder) = m {
                    // We may need to backtrack to this position.
                    // The next thing we should try is to take one character from the start of the match.
                    let needle_len = finder.needle().len();
                    let rem_len = rem.len();
                    let matched_at = remaining_input.len() - rem_len - needle_len;
                    let t = remaining_input.get(matched_at..).unwrap();
                    let next_try_input = strip_char(t);
                    last_wildcard = Some((remaining_matchers, next_try_input));
                    should_backtrack = false;
                }

                remaining_input = rem;
                remaining_matchers = &remaining_matchers[1..];
            } else if !remaining_input.is_empty() {
                match (m, last_wildcard) {
                    (SkipToLiteral(_), _) => {
                        // We failed to find the literal in the remaining input, so we fail.
                        return false;
                    }

                    (_, Some((matchers, next_try_input))) if should_backtrack => {
                        // We can backtrack!
                        remaining_matchers = matchers;
                        remaining_input = next_try_input;
                    }

                    _ => return false,
                }
            } else {
                return false;
            }

            if !remaining_input.is_empty() && remaining_matchers.is_empty() && should_backtrack {
                if let Some((matchers, next_try_input)) = last_wildcard {
                    // We can backtrack!
                    remaining_matchers = matchers;
                    remaining_input = next_try_input;
                } else {
                    return false;
                }
            }
        }

        // We have exhausted all matchers. See if there is any more input.
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
