use crate::patterns::{Pattern, Patterns};
use memchr::memmem::Finder;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::Deref;

#[derive(Clone)]
pub enum TerminalMatcher {
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

impl Debug for TerminalMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TerminalMatcher::*;
        match self {
            All => write!(f, "All"),
            End => write!(f, "End"),
            StartsWith(prefix) => write!(f, "StartsWith(\"{}\")", prefix),
            StartsWithChar(c) => write!(f, "StartsWithChar('{}')", c),
            EndsWith(suffix) => write!(f, "EndsWith(\"{}\")", suffix),
            EndsWithChar(c) => write!(f, "EndsWithChar('{}')", c),
            Contains(finder) => {
                write!(
                    f,
                    "Contains(\"{}\")",
                    std::str::from_utf8(finder.needle()).unwrap()
                )
            }
            Equals(s) => write!(f, "Equals(\"{}\")", s),
            EqualsChar(c) => write!(f, "EqualsChar('{}')", c),
            Len(n) => write!(f, "Len({})", n),
        }
    }
}

impl Display for TerminalMatcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
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

#[derive(Clone)]
pub enum MedialMatcher {
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
                chars.nth(n.get())?;
                Some(chars.as_str())
            }
        }
    }
}

impl Debug for MedialMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MedialMatcher::*;
        match self {
            Literal(s) => write!(f, "Literal(\"{}\")", s),
            LiteralChar(c) => write!(f, "LiteralChar('{}')", c),
            SkipToLiteral(finder) => write!(
                f,
                "SkipToLiteral(\"{}\")",
                std::str::from_utf8(finder.needle()).unwrap()
            ),
            Exactly(n) => write!(f, "Exactly({})", n),
        }
    }
}

impl Display for MedialMatcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum Matcher {
    Terminal(TerminalMatcher),
    Medial(MedialMatcher),
}

impl Matcher {
    fn from_pattern(p: Pattern) -> Self {
        use Pattern::*;

        match p {
            All => Matcher::Terminal(TerminalMatcher::All),

            End => Matcher::Terminal(TerminalMatcher::End),

            StartsWith(s) => {
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::StartsWithChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::StartsWith(s.to_string()))
                }
            }

            EndsWith(s) => {
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::EndsWithChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::EndsWith(s.to_string()))
                }
            }

            Contains(s) => Matcher::Terminal(TerminalMatcher::Contains(
                Finder::new(s.as_bytes()).into_owned(),
            )),

            Equals(s) => {
                if s.chars().count() == 1 {
                    Matcher::Terminal(TerminalMatcher::EqualsChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Terminal(TerminalMatcher::Equals(s.to_string()))
                }
            }

            Len(n) => Matcher::Terminal(TerminalMatcher::Len(NonZeroUsize::try_from(n).unwrap())),

            Literal(s) => {
                if s.chars().count() == 1 {
                    Matcher::Medial(MedialMatcher::LiteralChar(s.chars().next().unwrap()))
                } else {
                    Matcher::Medial(MedialMatcher::Literal(s.to_string()))
                }
            }

            SkipToLiteral(s) => Matcher::Medial(MedialMatcher::SkipToLiteral(
                Finder::new(s.as_bytes()).into_owned(),
            )),

            Exactly(n) => {
                Matcher::Medial(MedialMatcher::Exactly(NonZeroUsize::try_from(n).unwrap()))
            }

            AtLeast(_) => panic!("AtLeast should have been optimized away"),
        }
    }
}

impl Display for Matcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Matchers(Vec<Matcher>);

impl Deref for Matchers {
    type Target = [Matcher];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Matchers {
    pub fn from_patterns(p: Patterns) -> Self {
        let patterns_vec: Vec<Pattern> = p.into();
        let matchers = patterns_vec
            .into_iter()
            .map(Matcher::from_pattern)
            .collect();
        Matchers(matchers)
    }

    pub fn matches(&self, s: &str) -> bool {
        let mut s = s;
        for m in &self[..] {
            match m {
                Matcher::Terminal(tm) => {
                    return tm.matches(s);
                }

                Matcher::Medial(mm) => {
                    if let Some(rest) = mm.matches(s) {
                        s = rest;
                    } else {
                        return false;
                    }
                }
            }
        }
        s.is_empty()
    }
}

impl Display for Matchers {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
