use crate::patterns::{Pattern, Patterns};
use memchr::memmem::Finder;
use std::ops::Deref;

#[derive(Clone)]
pub enum TerminalMatcher {
    /// Consume the entire string.
    All,
    /// Consume the end of the string.
    End,
    /// Consume the entire string if it starts with the given prefix.
    StartsWith(String),
    /// Consume the entire string if it ends with the given suffix.
    EndsWith(String),
    /// Consume the entire string if it contains the given substring.
    Contains(Finder<'static>),
    /// Consume the entire string if it equals the given string.
    Equals(String),
    /// Consume the entire string if it has the given length.
    Len(usize),
}

impl TerminalMatcher {
    pub fn matches(&self, s: &str) -> bool {
        use TerminalMatcher::*;
        match self {
            All => true,
            End => s.is_empty(),
            StartsWith(prefix) => s.starts_with(prefix),
            EndsWith(suffix) => s.ends_with(suffix),
            Contains(finder) => finder.find(s.as_bytes()).is_some(),
            Equals(s2) => s == s2,
            Len(n) => s.chars().count() == *n,
        }
    }
}

#[derive(Clone)]
pub enum MedialMatcher {
    /// Consume a literal string.
    Literal(String),
    /// Consume any number of characters and match a literal.
    SkipToLiteral(Finder<'static>),
    /// Consume exactly the given number of characters.
    Exactly(usize),
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

            SkipToLiteral(finder) => finder.find(s.as_bytes()).map(|pos| &s[pos..]),

            Exactly(n) => {
                let mut chars = s.chars();
                for _ in 0..*n {
                    chars.next()?;
                }
                Some(chars.as_str())
            }
        }
    }
}

#[derive(Clone)]
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
            StartsWith(s) => Matcher::Terminal(TerminalMatcher::StartsWith(s.to_string())),
            EndsWith(s) => Matcher::Terminal(TerminalMatcher::EndsWith(s.to_string())),
            Contains(s) => Matcher::Terminal(TerminalMatcher::Contains(
                Finder::new(s.as_bytes()).into_owned(),
            )),
            Equals(s) => Matcher::Terminal(TerminalMatcher::Equals(s.to_string())),
            Len(n) => Matcher::Terminal(TerminalMatcher::Len(n)),
            Literal(s) => Matcher::Medial(MedialMatcher::Literal(s.to_string())),
            SkipToLiteral(s) => Matcher::Medial(MedialMatcher::SkipToLiteral(
                Finder::new(s.as_bytes()).into_owned(),
            )),
            Exactly(n) => Matcher::Medial(MedialMatcher::Exactly(n)),
            AtLeast(_) => panic!("AtLeast should have been optimized away"),
        }
    }
}

#[derive(Clone)]
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
