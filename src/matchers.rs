use crate::tokens::{Token, Tokens};
use std::borrow::Cow;
use std::ops::Deref;
use std::vec::IntoIter;

/// Matching units that can non-deterministically consume characters from a string.
#[derive(Debug, Clone)]
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
}

/// A sequence of matchers.
/// This can be thought of as the "Intermediate Representation" (IR) of the matching engine.
/// We run optimizations on it to reduce the search space during matching.
#[derive(Debug, Clone)]
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

                (Literal(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), AtLeast(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), Exactly(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (Literal(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), AtLeast(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), Exactly(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (SkipToLiteral(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Literal(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), SkipToLiteral(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), End) => {
                    v.push(a.clone());
                }

                (Exactly(_), All) => {
                    v.push(a.clone());
                }

                (Exactly(_), StartsWith(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), EndsWith(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Contains(_)) => {
                    v.push(a.clone());
                }

                (Exactly(_), Equals(_)) => {
                    v.push(a.clone());
                }

                (End | All | StartsWith(_) | EndsWith(_) | Contains(_) | Equals(_), _) => {
                    unreachable!("{:?} should always be the last matcher", a);
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
