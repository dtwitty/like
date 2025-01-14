use crate::tokens::{Token, Tokens};
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::slice::Iter;
use std::vec::IntoIter;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Clone, PartialEq, Eq)]
pub enum Matcher<'a> {
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
    /// Consume the entire string if it has the given length.
    Len(usize),
}

impl<'a> Debug for Matcher<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Matcher::*;
        match self {
            Literal(s) => write!(f, "Literal(\"{}\")", s),
            SkipToLiteral(s) => write!(f, "SkipToLiteral(\"{}\")", s),
            AtLeast(n) => write!(f, "AtLeast({})", n),
            Exactly(n) => write!(f, "Exactly({})", n),
            End => write!(f, "End"),
            All => write!(f, "All"),
            StartsWith(s) => write!(f, "StartsWith(\"{}\")", s),
            EndsWith(s) => write!(f, "EndsWith(\"{}\")", s),
            Contains(s) => write!(f, "Contains(\"{}\")", s),
            Equals(s) => write!(f, "Equals(\"{}\")", s),
            Len(n) => write!(f, "Len({})", n),
        }
    }
}

impl<'a> Matcher<'a> {
    #[cfg(test)]
    pub fn is_terminal(&self) -> bool {
        use Matcher::*;
        match self {
            End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_) => true,
            _ => false,
        }
    }
}

/// A sequence of matchers.
/// This can be thought of as the "Intermediate Representation" (IR) of the matching engine.
/// We run optimizations on it to reduce the search space during matching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matchers<'a> {
    matchers: Vec<Matcher<'a>>,
}

impl<'a> Matchers<'a> {
    pub fn from_tokens(tokens: Tokens<'a>) -> Matchers<'a> {
        let mut v = Vec::new();

        v.extend(tokens.into_iter().map(|token| match token {
            Token::Literal(s) => Matcher::Literal(Cow::Borrowed(s)),
            Token::Any => Matcher::AtLeast(0),
            Token::Single => Matcher::Exactly(1),
        }));

        v.push(Matcher::End);
        Matchers { matchers: v }
    }

    #[cfg(test)]
    fn from_vec(matchers: Vec<Matcher<'a>>) -> Self {
        Matchers { matchers }
    }

    pub fn as_vec(&self) -> &Vec<Matcher<'a>> {
        &self.matchers
    }

    pub fn optimize(self) -> Self {
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

                (Len(0), _) => {
                    v.push(End);
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

                (AtLeast(a), Len(b)) => {
                    v.push(Exactly(*a + *b));
                    v.push(All);
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

                (Exactly(a), End) => {
                    v.push(Len(*a));
                    i += 1;
                    changed = true;
                }

                (Exactly(a), Len(b)) => {
                    v.push(Len(*a + *b));
                    i += 1;
                    changed = true;
                }

                (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_), _) => {
                    unreachable!("{:?} should always be the last matcher", a);
                }

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

pub type MatchersIntoIter<'a> = IntoIter<Matcher<'a>>;

impl<'a> IntoIterator for Matchers<'a> {
    type Item = Matcher<'a>;
    type IntoIter = MatchersIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.matchers.into_iter()
    }
}

pub type MatchersIter<'a> = Iter<'a, Matcher<'a>>;

impl<'a> IntoIterator for &'a Matchers<'a> {
    type Item = &'a Matcher<'a>;
    type IntoIter = MatchersIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.matchers.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::Matcher::*;
    use super::*;
    use crate::tokens::lex;
    use proptest::prelude::ProptestConfig;
    use proptest::proptest;
    #[test]
    fn test_optimizations() {
        let tokens = lex("hello%");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(
            matchers,
            Matchers::from_vec(vec![StartsWith(Cow::Borrowed("hello"))])
        );

        let tokens = lex("hello%world");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(
            matchers,
            Matchers::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = lex("hello%world%");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(
            matchers,
            Matchers::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                Contains(Cow::Borrowed("world"))
            ])
        );

        let tokens = lex("%hello%world");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(
            matchers,
            Matchers::from_vec(vec![
                SkipToLiteral(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = lex("_%_%_");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(matchers, Matchers::from_vec(vec![Exactly(3), All]));

        let tokens = lex("a%b%c");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(
            matchers,
            Matchers::from_vec(vec![
                Literal(Cow::Borrowed("a")),
                SkipToLiteral(Cow::Borrowed("b")),
                EndsWith(Cow::Borrowed("c"))
            ])
        );

        let tokens = lex("");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(matchers, Matchers::from_vec(vec![End]));

        let tokens = lex("_");
        let matchers = Matchers::from_tokens(tokens).optimize();
        assert_eq!(matchers, Matchers::from_vec(vec![Len(1)]));
    }

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
            let m = matchers.matchers;

            // The matchers should never be empty.
            assert!(!m.is_empty());

            // The last matcher should always be terminal.
            assert!(m.last().unwrap().is_terminal());

            // Every other matcher should not be terminal.
            for i in 0..m.len() - 1 {
                assert!(!m[i].is_terminal());
            }

            // The matchers should not contain empty literals-like things.
            for i in 0..m.len() {
                match &m[i] {
                    Literal(s) if s.is_empty() => assert!(false),
                    SkipToLiteral(s) if s.is_empty() => assert!(false),
                    StartsWith(s) if s.is_empty() => assert!(false),
                    EndsWith(s) if s.is_empty() => assert!(false),
                    Contains(s) if s.is_empty() => assert!(false),
                    Equals(s) if s.is_empty() => assert!(false),
                    _ => (),
                }
            }

            // Helper matchers should be optimized away.
            for i in 0..m.len() {
                match &m[i] {
                    AtLeast(_) => assert!(false),
                    _ => (),
                }
            }

            // The matchers should not contain empty counters.
            for i in 0..m.len() {
                match &m[i] {
                    Exactly(0) => assert!(false),
                    AtLeast(0) => assert!(false),
                    _ => (),
                }
            }
        }
    }
}
