use crate::cat::Cat;
use crate::patterns::Pattern::{
    All, AtLeast, Contains, End, EndsWith, Equals, Exactly, Len, Literal, SkipToLiteral, StartsWith,
};
use crate::tokens::{Token, Tokens};
use std::ops::Deref;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern<'a> {
    /// Consume a literal string.
    Literal(Cat<'a>),
    /// Consume any number of characters and match a literal.
    SkipToLiteral(Cat<'a>),
    /// Consume any character, at least the given number of times.
    AtLeast(usize),
    /// Consume exactly the given number of characters.
    Exactly(usize),
    /// Consume the end of the string.
    End,
    /// Consume the entire string.
    All,
    /// Consume the entire string if it starts with the given prefix.
    StartsWith(Cat<'a>),
    /// Consume the entire string if it ends with the given suffix.
    EndsWith(Cat<'a>),
    /// Consume the entire string if it contains the given substring.
    Contains(Cat<'a>),
    /// Consume the entire string if it equals the given string.
    Equals(Cat<'a>),
    /// Consume the entire string if it has the given length.
    Len(usize),
}

pub(crate) enum MergeResult<'a> {
    /// Nothing changed, both patterns returned.
    Nothing(Pattern<'a>, Pattern<'a>),
    /// The 2 potterns were merged into 1.
    Merged(Pattern<'a>),
    /// Both patterns were changed and returned.
    Transformed(Pattern<'a>, Pattern<'a>),
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

    /// Merges this pattern with another one.
    /// Returns the number of patterns that were consumed.
    pub fn merge_with(self, other: Pattern<'a>) -> MergeResult<'a> {
        use MergeResult::*;

        match (self, other) {
            // Combine adjacent literals.
            // Escaping can cause literals to be split during Tokens::from_string.
            (Literal(mut a), Literal(b)) => {
                a.merge_with(b);
                Merged(Literal(a))
            }

            // Combine adjacent "skip to literal" and literal.
            (SkipToLiteral(mut a), Literal(b)) => {
                a.merge_with(b);
                Merged(SkipToLiteral(a))
            }

            // Combine "at least" and literal to make "skip to literal".
            // This allows us to use highly-optimized substring search algorithms.
            (AtLeast(a), Literal(b) | SkipToLiteral(b)) => {
                if a == 0 {
                    Merged(SkipToLiteral(b))
                } else {
                    Transformed(Exactly(a), SkipToLiteral(b))
                }
            }

            // Combine character counters like "___" into a single pattern.
            (Exactly(a), Exactly(b)) => Merged(Exactly(a + b)),

            // Combine any combination of "at least" and "exactly" counters.
            (AtLeast(a), Exactly(b)) | (AtLeast(a), AtLeast(b)) | (Exactly(a), AtLeast(b)) => {
                Merged(AtLeast(a + b))
            }

            (AtLeast(a), Len(b)) => Merged(Len(a + b)),

            // Optimizes for "match any ending" patterns like "hello%".
            (AtLeast(a), All | End) => {
                if a == 0 {
                    Merged(All)
                } else {
                    Transformed(Exactly(a), All)
                }
            }

            (AtLeast(a), Equals(b)) => {
                if a == 0 {
                    Merged(EndsWith(b))
                } else {
                    Transformed(Exactly(a), EndsWith(b))
                }
            }

            (AtLeast(a), Contains(b)) => {
                if a == 0 {
                    Merged(Contains(b))
                } else {
                    Transformed(Exactly(a), StartsWith(b))
                }
            }

            (AtLeast(a), StartsWith(b)) => {
                if a == 0 {
                    Merged(Contains(b))
                } else {
                    Transformed(Exactly(a), Contains(b))
                }
            }

            (AtLeast(a), EndsWith(b)) => {
                if a == 0 {
                    Merged(EndsWith(b))
                } else {
                    Transformed(Exactly(a), EndsWith(b))
                }
            }

            (Literal(s), End) => Merged(Equals(s)),

            (SkipToLiteral(s), End) => Merged(EndsWith(s)),

            (Literal(s), All) => Merged(StartsWith(s)),

            (SkipToLiteral(s), All) => Merged(Contains(s)),

            (Literal(mut a), StartsWith(b)) => {
                a.merge_with(b);
                Merged(StartsWith(a))
            }

            (Literal(mut a), Equals(b)) => {
                a.merge_with(b);
                Merged(Equals(a))
            }

            (SkipToLiteral(mut a), StartsWith(b)) => {
                a.merge_with(b);
                Merged(StartsWith(a))
            }

            (SkipToLiteral(mut a), Equals(b)) => {
                a.merge_with(b);
                Merged(EndsWith(a))
            }

            (Exactly(a), End) => Merged(Len(a)),

            (Exactly(a), Len(b)) => Merged(Len(a + b)),

            (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_), _) => {
                unreachable!("Terminal should always be the last pattern");
            }

            (a, b) => Nothing(a, b),
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
            Token::Literal(s) => Literal(Cat::from_str(s)),
            Token::Any => AtLeast(0),
            Token::Single => Exactly(1),
        }));

        v.push(End);
        Patterns(v)
    }

    #[cfg(test)]
    fn from_vec(patterns: Vec<Pattern<'a>>) -> Self {
        Patterns(patterns)
    }

    pub fn optimize(mut self) -> Self {
        while self.optimize_one() {}
        self
    }

    fn optimize_one(&mut self) -> bool {
        use MergeResult::*;
        use Pattern::*;

        if self.0.is_empty() {
            return false;
        }

        // Points just past the last element that we are done with.
        let mut done_idx = 0;

        // Points at the first element we are considering.
        let mut idx = 0;

        // Whether we changed anything (and therefore we should keep optimizing).
        let mut changed = false;

        while idx + 1 < self.0.len() {
            // Take ownership of this and the next element.
            // `Len(0)` is a noop element that is just a placeholder.
            let a = std::mem::replace(&mut self.0[idx], Len(0));
            let b = std::mem::replace(&mut self.0[idx + 1], Len(0));

            // Try to merge the two patterns.
            let merge_result = a.merge_with(b);
            match merge_result {
                Nothing(a, b) => {
                    // Finish the first element.
                    self.0[done_idx] = a;
                    done_idx += 1;

                    // Put the second element back.
                    self.0[idx + 1] = b;
                }

                Merged(p) => {
                    // Something changed!
                    changed = true;

                    // Put the merged element in the second element's slot.
                    // This effectively deletes the first element.
                    // It also allows further optimizations to be applied in sequence.
                    self.0[idx + 1] = p;
                }

                Transformed(a, b) => {
                    // Something changed!
                    changed = true;

                    // Call the first element done.
                    self.0[done_idx] = a;
                    done_idx += 1;

                    // Put the second element back so it can be merged with the next element.
                    self.0[idx + 1] = b;
                }
            }

            // Move the index forward.
            idx += 1;
        }

        // At this point, `idx` points to the last element.
        self.0.swap(done_idx, idx);
        done_idx += 1;

        // Dump all the elements that got optimized away.
        self.0.truncate(done_idx);

        changed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::ProptestConfig;
    use proptest::proptest;
    #[test]
    fn test_optimizations() {
        let tokens = Tokens::from_str("hello%");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![StartsWith(Cat::from_str("hello"))])
        );

        let tokens = Tokens::from_str("hello%world");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cat::from_str("hello")),
                EndsWith(Cat::from_str("world"))
            ])
        );

        let tokens = Tokens::from_str("hello%world%");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cat::from_str("hello")),
                Contains(Cat::from_str("world"))
            ])
        );

        let tokens = Tokens::from_str("%hello%world");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                SkipToLiteral(Cat::from_str("hello")),
                EndsWith(Cat::from_str("world"))
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
                Literal(Cat::from_str("a")),
                SkipToLiteral(Cat::from_str("b")),
                EndsWith(Cat::from_str("c"))
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
