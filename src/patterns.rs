//! This module contains the intermediate representation (IR) of the matching engine. Though the
//! module itself is non-public so it can change at any time, these docs are available for the
//! curious.
//!
//! # Patterns and Optimization
//! There are 2 concepts to understand here: rewriting and match-time optimizations.
//!
//! ## Rewriting
//! Rewrites turn a sequence of patterns into one that is more efficient to match. Here is a
//! (non-exhaustive) list of rules implemented by this crate:
//! - Adjacent literals are combined into one.
//! - Adjacent "any" tokens are merged.
//! - Adjacent "single" tokens are merged into a single "exactly n characters" token.
//! - A "single" token can be pushed left past an "any" token, letting us consume characters as
//!  early as possible.
//! - "Any", "single" can be merged into "at least n characters".
//! - "Any" can be combined with a literal to make a special "skip to literal" pattern.
//!
//! This crate also has special patterns for the end of the string. For example:
//!  - `"...%abc"` can be matched as "ends with `"abc"`".
//! - `"...%abc%"` can be matched as "ends with a string containing `"abc"`".
//! - `"...abc%"` can be matched as "ends with a string starting with`"abc"`".
//! - `"..._____"` can be matched as "ends with a string of length 5".
//! - etc...
//!
//! These end matchers speed up the matching process by allowing us to ignore lots of characters.
//! For example, the pattern `"hello%word"` can be matched by only looking at the first and last few
//! characters of the string, even if the string is very long.
//!
//! ## Match-Time Optimizations
//! At match time, we implement a simple state machine that consumes characters from the string. The
//! real workhorse is [`Finder`](https://docs.rs/memchr/latest/memchr/memmem/struct.Finder.html)
//! struct from the [`memchr`](https://docs.rs/memchr/latest/memchr/index.html) crate, which allows
//! us to search for substrings extremely quickly. There are optimizations for dealing with single
//! characters vs substrings.

use crate::cat::Cat;
use crate::patterns::MergeResult::*;
use crate::patterns::Pattern::*;
use crate::tokens::{Token, Tokens};
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::Deref;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern<'a> {
    /// Consume a literal string.
    Literal(Cat<'a>),
    EndLiteral(Cat<'a>),
    /// Consume any number of characters and match a literal.
    SkipToLiteral(Cat<'a>),
    /// Consume any character, at least the given number of times.
    AtLeast(usize),
    /// Consume exactly the given number of characters.
    Exactly(NonZeroUsize),
    EndExactly(NonZeroUsize),
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
    Len(NonZeroUsize),
    /// Consume nothing. This is a placeholder for optimizations.
    Noop,
}

pub(crate) enum MergeResult<'a> {
    /// Nothing changed, both patterns returned.
    Nothing(Pattern<'a>, Pattern<'a>),
    /// The 2 patterns were merged into 1.
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
            (Noop, other) => Merged(other),
            (other, Noop) => Merged(other),
            // Combine adjacent literals.
            // Escaping can cause literals to be split during Tokens::from_string.
            (Literal(a), Literal(b)) => Merged(Literal(a.merge_with(b))),

            // We merge end literals in reverse to preserve the order of the string.
            (EndLiteral(a), EndLiteral(b)) => Merged(EndLiteral(b.merge_with(a))),

            // Combine adjacent "skip to literal" and literal.
            (SkipToLiteral(a), Literal(b)) => Merged(SkipToLiteral(a.merge_with(b))),

            // Combine "at least" and literal to make "skip to literal".
            // This allows us to use highly-optimized substring search algorithms.
            (AtLeast(a), Literal(c) | SkipToLiteral(c)) => {
                maybe_prepend_exactly(a, SkipToLiteral(c))
            }

            // Combine character counters like "___" into a single pattern.
            (Exactly(a), Exactly(b)) => Merged(Exactly(a.checked_add(b.get()).unwrap())),
            (EndExactly(a), EndExactly(b)) => Merged(EndExactly(a.checked_add(b.get()).unwrap())),

            // Combine any combination of "at least" and "exactly" counters.
            (AtLeast(a), Exactly(b)) => Merged(AtLeast(a + b.get())),

            (AtLeast(a), AtLeast(b)) => Merged(AtLeast(a + b)),

            (Exactly(a), AtLeast(b)) => Merged(AtLeast(a.get() + b)),

            (AtLeast(a), Len(b)) => Transformed(Exactly(b.checked_add(a).unwrap()), All),

            // Optimizes for "match any ending" patterns like "hello%".
            (AtLeast(a), All | End) => maybe_prepend_exactly(a, All),

            (AtLeast(a), Equals(c)) => maybe_prepend_exactly(a, EndsWith(c)),

            (AtLeast(a), Contains(c)) => maybe_prepend_exactly(a, Contains(c)),

            (AtLeast(a), StartsWith(c)) => maybe_prepend_exactly(a, Contains(c)),

            (AtLeast(a), EndsWith(c)) => maybe_prepend_exactly(a, EndsWith(c)),

            (Literal(s), End) => Merged(Equals(s)),

            (SkipToLiteral(s), End) => Merged(EndsWith(s)),

            (Literal(s), All) => Merged(StartsWith(s)),

            (SkipToLiteral(s), All) => Merged(Contains(s)),

            (Literal(a), StartsWith(b)) => Merged(StartsWith(a.merge_with(b))),

            (Literal(a), Equals(b)) => Merged(Equals(a.merge_with(b))),

            (SkipToLiteral(a), StartsWith(b)) => Merged(StartsWith(a.merge_with(b))),

            (SkipToLiteral(a), Equals(b)) => Merged(EndsWith(a.merge_with(b))),

            (Exactly(a), End) => Merged(Len(a)),

            (Exactly(a), Len(b)) => Merged(Len(a.checked_add(b.get()).unwrap())),

            (EndLiteral(s), End) => Merged(Equals(s)),

            (EndLiteral(s), All) => Merged(EndsWith(s)),

            (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_) | Len(_), _) => {
                unreachable!("Terminal should always be the last pattern");
            }

            (a, b) => Nothing(a, b),
        }
    }
}

fn maybe_prepend_exactly(n: usize, p: Pattern) -> MergeResult {
    match NonZeroUsize::new(n) {
        Some(n) => Transformed(Exactly(n), p),
        None => Merged(p),
    }
}

impl<'a> Display for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Pattern::*;
        match self {
            Literal(s) => write!(f, "Literal({:?})", s.to_string()),
            SkipToLiteral(s) => write!(f, "SkipToLiteral({:?})", s.to_string()),
            AtLeast(n) => write!(f, "AtLeast({:?})", n),
            Exactly(n) => write!(f, "Exactly({:?})", n),
            End => write!(f, "End"),
            All => write!(f, "All"),
            StartsWith(s) => write!(f, "StartsWith({:?})", s.to_string()),
            EndsWith(s) => write!(f, "EndsWith({:?})", s.to_string()),
            Contains(s) => write!(f, "Contains({:?})", s.to_string()),
            Equals(s) => write!(f, "Equals({:?})", s.to_string()),
            Len(n) => write!(f, "Len({:?})", n),
            Noop => write!(f, "Noop"),
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
            Token::Single => Exactly(NonZeroUsize::new(1).unwrap()),
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
        while self.optimize_end() {}
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
            let a = std::mem::replace(&mut self.0[idx], Noop);
            let b = std::mem::replace(&mut self.0[idx + 1], Noop);

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

    fn optimize_end(&mut self) -> bool {
        use Pattern::*;

        // Split to the slice into segments starting with SkipToLiteral.
        let mut segments = vec![vec![]];
        for p in &self.0 {
            match p {
                p @ SkipToLiteral(_) => segments.push(vec![p.clone()]),
                _ => segments.last_mut().unwrap().push(p.clone()),
            }
        }

        if segments.len() < 2 {
            // There's no skipping to do.
            return false;
        }

        // We are only interested in the first and last segments.
        let (first, x) = segments.split_first_mut().unwrap();
        let last = x.split_last_mut().unwrap().0;

        let mut changed = false;
        while match &last[..] {
            [.., _, EndsWith(s)] => {
                // We can merge the last segment into the first one.
                first.push(EndLiteral(s.clone()));
                let n = last.len() - 1;
                last[n] = All;
                true
            }

            [.., _, Equals(s)] => {
                // We can merge the last segment into the first one.
                first.push(EndLiteral(s.clone()));
                let n = last.len() - 1;
                last[n] = End;
                true
            }

            [.., _, Len(n)] => {
                // We can merge the last segment into the first one.
                first.push(EndExactly(*n));
                let n = last.len() - 1;
                last[n] = End;
                true
            }

            [.., Literal(s), End] => {
                // We can merge the last segment into the first one.
                first.push(EndLiteral(s.clone()));
                last.pop();
                let n = last.len() - 1;
                last[n] = End;
                true
            }

            [.., Exactly(x), End] => {
                let n = last.len() - 1;
                last[n - 1] = Len(*x);
                last.pop();
                true
            }

            [.., Literal(s), All] => {
                // We can merge the last segment into the first one.
                let n = last.len() - 1;
                last[n - 1] = StartsWith(s.clone());
                last.pop();
                true
            }

            [.., SkipToLiteral(s), End] => {
                // We can merge the last segment into the first one.
                let n = last.len() - 1;
                last[n - 1] = EndsWith(s.clone());
                last.pop();
                true
            }

            [.., SkipToLiteral(s), All] => {
                // We can merge the last segment into the first one.
                let n = last.len() - 1;
                last[n - 1] = Contains(s.clone());
                last.pop();
                true
            }

            _ => false,
        } {
            changed = true;
        }

        self.0 = segments.iter().flatten().cloned().collect();
        changed
    }
}

impl<'a> Display for Patterns<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        if let Some((last, rest)) = self.0.split_last() {
            for p in rest {
                write!(f, "{}, ", p)?;
            }
            write!(f, "{}", last)?;
        }
        write!(f, "]")?;
        Ok(())
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
                EndLiteral(Cat::from_str("world")),
                Contains(Cat::from_str("hello"))
            ])
        );

        let tokens = Tokens::from_str("_%_%_");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![Exactly(NonZeroUsize::new(3).unwrap()), All])
        );

        let tokens = Tokens::from_str("a%b%c");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![
                Literal(Cat::from_str("a")),
                EndLiteral(Cat::from_str("c")),
                Contains(Cat::from_str("b"))
            ])
        );

        let tokens = Tokens::from_str("");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(patterns, Patterns::from_vec(vec![End]));

        let tokens = Tokens::from_str("_");
        let patterns = Patterns::from_tokens(&tokens).optimize();
        assert_eq!(
            patterns,
            Patterns::from_vec(vec![Len(NonZeroUsize::new(1).unwrap())])
        );
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
                    AtLeast(0) => assert!(false),
                    _ => (),
                }
            }
        }
    }

    #[test]
    fn snapshot_test() {
        let patterns = [
            "",
            "%",
            "_",
            "_x_",
            "________",
            "hello",
            "%hello%",
            "x__y",
            "%abc",
            "abc%",
            "lo%rld",
            "%hello%to%the%world",
            "%hello%to%the%world%",
            "%hello%to%the%__world%",
            "%hello%to%the%__world__%",
            "%hello%to%the%__world__",
            "%hello%to%the%__world%__",
        ];

        for s in patterns {
            let tokens = Tokens::from_str(s);
            let patterns = Patterns::from_tokens(&tokens).optimize();
            let snap = format!("({:?}, {})", s, patterns);
            insta::assert_snapshot!(s, snap);
        }
    }
}
