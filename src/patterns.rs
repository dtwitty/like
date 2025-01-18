use crate::tokens::{Token, Tokens};
use std::borrow::Cow;
use std::ops::Deref;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern<'a> {
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
pub(crate) struct Patterns<'a>(Vec<Pattern<'a>>);

impl<'a> Deref for Patterns<'a> {
    type Target = [Pattern<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> Patterns<'a> {
    pub fn from_tokens(tokens: &Tokens<'a>) -> Patterns<'a> {
        let mut v = Vec::with_capacity(tokens.len());

        v.extend(tokens.iter().map(|token| match token {
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
        let mut next_vec = Vec::with_capacity(self.len());
        let mut curr = self;
        loop {
            match curr.optimize_one(next_vec) {
                (Ok(patterns), mut v) => {
                    curr = patterns;
                    v.clear();
                    next_vec = v;
                }
                (Err(patterns), _) => return patterns,
            }
        }
    }

    fn optimize_one(
        mut self,
        mut next_vec: Vec<Pattern<'a>>,
    ) -> (Result<Self, Self>, Vec<Pattern<'a>>) {
        use Pattern::*;

        if self.0.is_empty() {
            return (Err(Patterns(self.0)), next_vec);
        }

        let mut changed = false;

        let mut it = self.0.drain(..).peekable();
        while let Some(a) = it.next() {
            let b = it.peek();
            if b.is_none() {
                next_vec.push(a);
                break;
            }
            let b = b.unwrap();

            match (a, b) {
                // Remove empty literals.
                (Literal(s), _) if s.is_empty() => {
                    it.next();
                    changed = true;
                }

                // Remove empty character counters.
                (Exactly(0), _) => {
                    it.next();
                    changed = true;
                }

                (Len(0), _) => {
                    next_vec.push(End);
                    it.next();
                    changed = true;
                }

                // Remove empty "skip to literal".
                (SkipToLiteral(s), _) if s.is_empty() => {
                    it.next();
                    changed = true;
                }

                // Combine adjacent literals.
                // Escaping can cause literals to be split during Tokens::from_string.
                (Literal(a), Literal(_)) => {
                    let Some(Literal(b)) = it.next() else {
                        unreachable!()
                    };
                    let mut a = a.into_owned();
                    a.push_str(&b.into_owned());
                    let c = Cow::Owned(a);
                    next_vec.push(Literal(c));
                    changed = true;
                }

                // Combine adjacent "skip to literal" and literal.
                (SkipToLiteral(a), Literal(_)) => {
                    let Some(Literal(b)) = it.next() else {
                        unreachable!()
                    };
                    let mut a = a.into_owned();
                    a.push_str(&b.into_owned());
                    let c = Cow::Owned(a);
                    next_vec.push(SkipToLiteral(c));
                    changed = true;
                }

                // Combine "at least" and literal to make "skip to literal".
                // This allows us to use highly-optimized substring search algorithms.
                (AtLeast(a), Literal(_) | SkipToLiteral(_)) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    let Some(SkipToLiteral(b) | Literal(b)) = it.next() else {
                        unreachable!()
                    };
                    next_vec.push(SkipToLiteral(b));
                    changed = true;
                }

                // Combine character counters like "___" into a single pattern.
                (Exactly(a), Exactly(b)) => {
                    next_vec.push(Exactly(a + b));
                    it.next();
                    changed = true;
                }

                // Combine any combination of "at least" and "exactly" counters.
                (AtLeast(a), Exactly(b)) | (AtLeast(a), AtLeast(b)) | (Exactly(a), AtLeast(b)) => {
                    next_vec.push(AtLeast(a + b));
                    it.next();
                    changed = true;
                }

                (AtLeast(a), Len(b)) => {
                    next_vec.push(Exactly(a + *b));
                    next_vec.push(All);
                    it.next();
                    changed = true;
                }

                // Optimizes for "match any ending" patterns like "hello%".
                (AtLeast(a), All | End) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    next_vec.push(All);
                    it.next();
                    changed = true;
                }

                (AtLeast(a), Equals(_)) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    let Some(Equals(s)) = it.next() else {
                        unreachable!()
                    };
                    next_vec.push(EndsWith(s));
                    changed = true;
                }

                (AtLeast(a), Contains(_)) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    let Some(Contains(s)) = it.next() else {
                        unreachable!()
                    };
                    next_vec.push(Contains(s));
                    changed = true;
                }

                (AtLeast(a), StartsWith(_)) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    let Some(StartsWith(s)) = it.next() else {
                        unreachable!()
                    };
                    next_vec.push(Contains(s));
                    changed = true;
                }

                (AtLeast(a), EndsWith(_)) => {
                    if a > 0 {
                        next_vec.push(Exactly(a));
                    }
                    let Some(EndsWith(s)) = it.next() else {
                        unreachable!()
                    };
                    next_vec.push(EndsWith(s.clone()));
                    changed = true;
                }

                (Literal(s), End) => {
                    next_vec.push(Equals(s));
                    it.next();
                    changed = true;
                }

                (SkipToLiteral(s), End) => {
                    next_vec.push(EndsWith(s));
                    it.next();
                    changed = true;
                }

                (Literal(s), All) => {
                    next_vec.push(StartsWith(s));
                    it.next();
                    changed = true;
                }

                (SkipToLiteral(s), All) => {
                    next_vec.push(Contains(s));
                    it.next();
                    changed = true;
                }

                (Literal(a), StartsWith(_)) => {
                    let mut a = a.into_owned();
                    let Some(StartsWith(b)) = it.next() else {
                        unreachable!()
                    };
                    a.push_str(b.deref());
                    let c = Cow::Owned(a);
                    next_vec.push(StartsWith(c));
                    changed = true;
                }

                (Literal(a), Equals(_)) => {
                    let mut a = a.into_owned();
                    let Some(Equals(b)) = it.next() else {
                        unreachable!()
                    };
                    a.push_str(b.deref());
                    let c = Cow::Owned(a);
                    next_vec.push(Equals(c));
                    changed = true;
                }

                (SkipToLiteral(a), StartsWith(_)) => {
                    let mut a = a.into_owned();
                    let Some(StartsWith(b)) = it.next() else {
                        unreachable!()
                    };
                    a.push_str(b.deref());
                    let c = Cow::Owned(a);
                    next_vec.push(StartsWith(c));
                    changed = true;
                }

                (SkipToLiteral(a), Equals(_)) => {
                    let mut a = a.into_owned();
                    let Some(Equals(b)) = it.next() else {
                        unreachable!()
                    };
                    a.push_str(b.deref());
                    let c = Cow::Owned(a);
                    next_vec.push(EndsWith(c));
                    changed = true;
                }

                (Exactly(a), End) => {
                    next_vec.push(Len(a));
                    it.next();
                    changed = true;
                }

                (Exactly(a), Len(b)) => {
                    next_vec.push(Len(a + *b));
                    it.next();
                    changed = true;
                }

                (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_), _) => {
                    unreachable!("Terminal should always be the last pattern");
                }

                (a, _) => {
                    next_vec.push(a);
                }
            }
        }

        let ret = Patterns(next_vec);
        drop(it);
        if changed {
            (Ok(ret), self.0)
        } else {
            (Err(ret), self.0)
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
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![StartsWith(Cow::Borrowed("hello"))])
        );

        let tokens = Tokens::from_str("hello%world");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("hello%world%");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("hello")),
                Contains(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("%hello%world");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                SkipToLiteral(Cow::Borrowed("hello")),
                EndsWith(Cow::Borrowed("world"))
            ])
        );

        let tokens = Tokens::from_str("_%_%_");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![Exactly(3), All]));

        let tokens = Tokens::from_str("a%b%c");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cow::Borrowed("a")),
                SkipToLiteral(Cow::Borrowed("b")),
                EndsWith(Cow::Borrowed("c"))
            ])
        );

        let tokens = Tokens::from_str("");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![End]));

        let tokens = Tokens::from_str("_");
        let patterns = Patterns::from_tokens(&tokens).optimize();
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
            let patterns = Patterns::from_tokens(&tokens).optimize();

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
