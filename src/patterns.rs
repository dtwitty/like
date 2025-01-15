use crate::tokens::{Token, Tokens};
use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Clone, PartialEq, Eq)]
pub enum Pattern<'a> {
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

impl<'a> Debug for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Pattern::*;
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

impl<'a> Display for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Pattern<'a> {
    #[cfg(test)]
    pub fn is_terminal(&self) -> bool {
        use Pattern::*;
        match self {
            End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_) => true,
            _ => false,
        }
    }
}

/// A sequence of patterns.
/// This can be thought of as the "Intermediate Representation" (IR) of the matching engine.
/// We run optimizations on it to reduce the search space during matching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Patterns<'a>(Vec<Pattern<'a>>);

impl<'a> Display for Patterns<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Deref for Patterns<'a> {
    type Target = [Pattern<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> From<Patterns<'a>> for Vec<Pattern<'a>> {
    fn from(value: Patterns<'a>) -> Self {
        value.0
    }
}

impl<'a> Patterns<'a> {
    pub fn from_tokens(tokens: Tokens<'a>) -> Patterns<'a> {
        let mut v = Vec::new();

        v.extend(tokens.into_iter().map(|token| match token {
            Token::Literal(s) => Pattern::Literal(Cow::Borrowed(s)),
            Token::Any => Pattern::AtLeast(0),
            Token::Single => Pattern::Exactly(1),
        }));

        v.push(Pattern::End);
        Patterns(v)
    }

    #[cfg(test)]
    fn from_vec(patterns: Vec<Pattern<'a>>) -> Self {
        Patterns(patterns)
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
        use Pattern::*;
        let mut v = Vec::new();
        let mut changed = false;

        let mut i = 0;
        while i + 1 < self.len() {
            let a = &self[i];
            let b = &self[i + 1];

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
                // Escaping can cause literals to be split during Tokens::from_string.
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

                // Combine character counters like "___" into a single pattern.
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
                    unreachable!("{:?} should always be the last pattern", a);
                }

                _ => {
                    v.push(a.clone());
                }
            }

            i += 1;
        }

        if i < self.len() {
            v.push(self[i].clone());
        }

        let ret = Patterns(v);
        if changed {
            Ok(ret)
        } else {
            Err(ret)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Pattern::*;
    use super::*;
    use proptest::prelude::ProptestConfig;
    use proptest::proptest;
    #[test]
    fn test_optimizations() {
        let tokens = Tokens::from_str("hello%");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![StartsWith(Cow::Borrowed("hello"))])
        );

        let tokens = Tokens::from_str("hello%world");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("hello%world%");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                Contains(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("%hello%world");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                SkipToLiteral(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("_%_%_");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![Exactly(3), All]));

        let tokens = Tokens::from_str("a%b%c");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("a")),
                SkipToLiteral(Cow::Borrowed("b")),
                EndsWith(Cow::Borrowed("c"))
            ])
        );

        let tokens = Tokens::from_str("");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![End]));

        let tokens = Tokens::from_str("_");
        let patterns = Patterns::from_tokens(tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![Len(1)]));
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 14,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_invariants(pattern in ".*") {
            let tokens = Tokens::from_str(&pattern);
            let patterns = Patterns::from_tokens(tokens).optimize();

            // The patterns should never be empty.
            assert!(!patterns.is_empty());

            // The last pattern should always be terminal.
            assert!(patterns.last().unwrap().is_terminal());

            // Every other pattern should not be terminal.
            for i in 0..patterns.len() - 1 {
                assert!(!patterns[i].is_terminal());
            }

            // The patterns should not contain empty literals-like things.
            for i in 0..patterns.len() {
                match &patterns[i] {
                    Literal(s) if s.is_empty() => assert!(false),
                    SkipToLiteral(s) if s.is_empty() => assert!(false),
                    StartsWith(s) if s.is_empty() => assert!(false),
                    EndsWith(s) if s.is_empty() => assert!(false),
                    Contains(s) if s.is_empty() => assert!(false),
                    Equals(s) if s.is_empty() => assert!(false),
                    _ => (),
                }
            }

            // Helper patterns should be optimized away.
            for i in 0..patterns.len() {
                match &patterns[i] {
                    AtLeast(_) => assert!(false),
                    _ => (),
                }
            }

            // The patterns should not contain empty counters.
            for i in 0..patterns.len() {
                match &patterns[i] {
                    Exactly(0) => assert!(false),
                    AtLeast(0) => assert!(false),
                    _ => (),
                }
            }
        }
    }
}
