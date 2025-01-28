//! A pattern matcher inspired by the SQL `LIKE` operator.
//! The primary type is [LikeMatcher], which can be used to match strings against a `LIKE` pattern.
//!
//! # Syntax
//! A `LIKE` pattern is composed of:
//!  - `%`: Matches any sequence of characters, including an empty sequence.
//!  - `_`: Matches any single character (a Unicode scalar value).
//!  - Literal strings
//!  - Escaped characters:
//!     - `"\\"` escapes a backslash, matching a single backslash/
//!     - `"\_"` escapes an underscore, matching a single underscore.
//!     - `"\%"` escapes a percent sign, matching a single percent sign.
//!     - Any other escaped character matches both the backslash and the character.
//!         - For example, `"\a"` matches a backslash followed by an `'a'`.
//!
//! ## Examples
//! For example:
//! - `"hello%"` matches any string that starts with "hello".
//! - `"%world"` matches any string that ends with "world".
//! - `"%hello%"` matches any string that contains "hello".
//! - `"hello"` matches the string "hello".
//! - `"%____"` matches any string that is at least 4 characters long.
//! - `"a_b"` matches the `"a"`, followed by any character, followed by `"b"`.
//! - `"a\\_b"` matches the string `"a_b"`.
//!
//! # Why not Regex?
//! You could certainly implement `LIKE`-style matching using the excellent
//! [`regex`](https://docs.rs/regex/latest/regex/) crate, and that will probably meet your needs.
//! That crate is much more flexible than this one.
//! However, there are a few reasons you might want to use this crate instead:
//!
//! ## Performance
//! Because `LIKE` patterns are much simpler than regular expressions, we can use highly optimized
//! routines from the [`memchr`](https://docs.rs/memchr/latest/memchr/) crate to quickly scan the
//! input string.
//!
//! There are also optimizations available for common patterns. For example:
//!  - `"%world"` checks the end of the input string.
//! - `"hello%"` checks the start of the input string.
//! - `"%hello%"` checks if the input string contains `"hello"`.
//! - `"hello"` checks if the input string is equal to `"hello"`.
//! - and many more!
//!
//! Finally, a [LikeMatcher] instance is often 10-100x faster to compile than a [Regex](https://docs.rs/regex/latest/regex/struct.Regex.html). This may be
//! beneficial in applications where you need to compile many patterns dynamically.
//!
//! N.B. The `regex` crate tends to be slightly faster at dealing the short patterns containing
//! "single character" tokens, possibly due to that crate using byte-based DFAs and this crate using
//! char-based state machines. However, for dealing with "any" tokens, this crate is almost always
//! faster.
//!
//! ## Transparency
//! This crate exposes its tokenizer, which allows building and accessing the parts of a pattern.
//! This allows building token patterns without needing to construct the actual pattern string,
//! making escaping and concatenation trivial.
//!
//! Having access to parsed tokens also allows optimizations for the caller. For example, database
//! engines that use a sorted index can look at the first token to narrow down the part of the index
//! to search.
//!
//! ## Reliability
//! All valid utf-8 strings are valid `LIKE` patterns.
//! This crate is also tested against a regex-based implementation to ensure correctness.
//! It uses fuzz testing to ensure that it can handle any well-formed `&str`.
//!
//! # How It Works
//! First, you compile a [LikeMatcher]. Behind the scenes, this will:
//! 1. Tokenize the pattern string, or use the supplied set of tokens.
//! 2. Transform the tokens to an intermediate representation (IR) for optimization.
//! 3. Apply optimizations to the IR, for example:
//!     - `"...%abc..."` becomes "skip to the next `"abc"`"
//!     - `"...%"` becomes "skip to the end"
//! 4. Transform the optimized IR to highly optimized substring matchers.
//!
//! Once you have a compiled `LikeMatcher`, you can use it to match strings.
//! The matcher is completely thread safe, though it is not cheap to clone.
//!
//! ## Matching Algorithm and Performance
//! This crate uses a modified version of
//! [Kurt's Iterative Wildcard Matching Algorithm](http://dodobyte.com/wildcard.html). Like that
//! algorithm, it has a worst-case time complexity of `O(n*m)`, where `n` is the length of the pattern
//! and `m` is the length of the input string. However, there are 2 major optimizations:
//! 1. It only backtracks unless it can prove that it is necessary, which boils down to "was there
//! a "single character" token in the pattern since the last "any" token?"
//! 2. It uses highly optimized substring matching routines to scan the input string.
//!
//! With these optimizations, inputs that contain no "single character" tokens are matched in `O(m)`
//! time, and many other inputs are matched much faster than Kurt's original algorithm.
//!
//! This of course comes at the cost of some preprocessing time. However, this time is typically
//! measured in hundreds of nanoseconds, and is almost always less than the time it takes to compile
//! a regex. For those interested, the optimizations this crate uses are described in [patterns] module.
mod cat;
mod matchers;
pub mod patterns;
pub mod tokens;

use crate::matchers::Matchers;
use crate::patterns::*;
use crate::tokens::*;

#[derive(Debug, Clone)]
pub struct LikeMatcher {
    matchers: Matchers,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = Tokens::from_str(s);
        let patterns = Patterns::from_tokens(&tokens).optimize();
        let matchers = Matchers::from_patterns(&patterns);
        LikeMatcher { matchers }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.matchers.matches(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Token::{Any, Literal, Single};
    use proptest::prelude::*;

    #[test]
    fn test_empty() {
        assert!(LikeMatcher::new("").matches(""));
        assert!(!LikeMatcher::new("").matches("world"));
    }

    #[test]
    fn test_literal() {
        assert!(!LikeMatcher::new("hello").matches(""));
        assert!(LikeMatcher::new("world").matches("world"));
        assert!(!LikeMatcher::new("hello").matches("world"));
    }

    #[test]
    fn test_starts_with() {
        assert!(!LikeMatcher::new("hello%").matches(""));
        assert!(LikeMatcher::new("hello%").matches("hello world"));
        assert!(!LikeMatcher::new("hello%").matches("world"));
    }

    #[test]
    fn test_ends_with() {
        assert!(!LikeMatcher::new("%world").matches(""));
        assert!(LikeMatcher::new("%world").matches("hello world"));
        assert!(!LikeMatcher::new("%world").matches("hello"));
    }

    #[test]
    fn test_contains() {
        assert!(!LikeMatcher::new("%hello%").matches(""));
        assert!(LikeMatcher::new("%world%").matches("hello world"));
        assert!(LikeMatcher::new("%hello%").matches("hello world"));
        assert!(LikeMatcher::new("%llo wo%").matches("hello world"));
        assert!(!LikeMatcher::new("%world%").matches("hello"));
        assert!(LikeMatcher::new("%🔥%").matches("hello 🔥"));
        assert!(LikeMatcher::new("%🔥%").matches("🔥hello"));
    }

    #[test]
    fn test_single() {
        assert!(!LikeMatcher::new("_").matches(""));
        assert!(LikeMatcher::new("_").matches("w"));
        assert!(!LikeMatcher::new("_").matches("he"));
        assert!(LikeMatcher::new("_").matches("🔥"));
        assert!(LikeMatcher::new("_").matches("¡"));
        assert!(LikeMatcher::new("_______________________").matches("aaaaaaaaaaaaaaaaaaaaaaa"));
        assert!(LikeMatcher::new("h_llo").matches("hello"));
        assert!(!LikeMatcher::new("h_llo").matches("world"));
        assert!(LikeMatcher::new("h_llo").matches("h🔥llo"));
    }

    #[test]
    fn test_any() {
        assert!(LikeMatcher::new("%").matches("hello world"));
        assert!(LikeMatcher::new("%%").matches("hello world"));
        assert!(LikeMatcher::new("%").matches(""));
        assert!(LikeMatcher::new("%%").matches(""));
    }

    #[test]
    fn test_any_single() {
        assert!(!LikeMatcher::new("h_%o").matches("ho"));
        assert!(!LikeMatcher::new("%_").matches(""));
        assert!(!LikeMatcher::new("_%").matches(""));
        assert!(LikeMatcher::new("%_").matches("hello world"));
        assert!(LikeMatcher::new("_%").matches("hello world"));
        assert!(LikeMatcher::new("%_").matches("h"));
        assert!(LikeMatcher::new("_%").matches("h"));
        assert!(LikeMatcher::new("h_%o").matches("hello"));
        assert!(LikeMatcher::new("h%_o").matches("hello"));
        assert!(LikeMatcher::new("h_%o").matches("hlo"));
        assert!(LikeMatcher::new("h%_o").matches("hlo"));
        assert!(!LikeMatcher::new("h%_o").matches("ho"));
        assert!(!LikeMatcher::new("h_%o").matches("world"));
        assert!(!LikeMatcher::new("h%_o").matches("world"));
    }

    #[test]
    fn test_escape() {
        assert!(LikeMatcher::new(r"hello\%").matches("hello%"));
        assert!(LikeMatcher::new(r"hello\_").matches("hello_"));
        assert!(!LikeMatcher::new(r"hello\%").matches("hello"));
        assert!(!LikeMatcher::new(r"hello\_").matches("hello"));
        assert!(LikeMatcher::new(r"hel\\o%").matches("hel\\o"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hel\\p"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hel\\"));
        assert!(!LikeMatcher::new(r"hel\o%").matches("hl\\o"));
        assert!(LikeMatcher::new(r"h\%o").matches("h%o"));
    }

    #[test]
    fn test_greediness() {
        assert!(LikeMatcher::new("'%'%%'%'").matches("'a'a'a'a'a'a'a'a'a'"));
        assert!(LikeMatcher::new("'%'").matches("' 'hello' world'"));
        assert!(LikeMatcher::new("a '%' b c").matches("a 'd' b c 'd' b c"));
    }

    #[test]
    fn test_regressions() {
        assert!(LikeMatcher::new("%a_b%b").matches("aabab"));
        assert!(LikeMatcher::new("%a_a_%b").matches("aaabab"));
        assert!(LikeMatcher::new("%b_").matches("bba"));
        assert!(!LikeMatcher::new("%ab___").matches("baaaab"));
        assert!(!LikeMatcher::new("%a").matches("ab"));
        assert!(!LikeMatcher::new("%aa_a").matches("aabb"));
        assert!(LikeMatcher::new("%ab__bb%").matches("abaabb"));
    }

    struct RegexLikeMatcher {
        regex: regex::Regex,
    }

    impl RegexLikeMatcher {
        fn new(pattern: &str) -> Self {
            let tokens = Tokens::from_str(pattern);
            let mut re = String::new();
            re.push('^');
            for token in &tokens[..] {
                match token {
                    Any => re.push_str(".*"),
                    Single => re.push_str("."),
                    Literal(lit) => re.push_str(&regex::escape(lit)),
                }
            }
            re.push('$');
            Self {
                regex: regex::Regex::new(&re).unwrap(),
            }
        }

        fn matches(&self, input: &str) -> bool {
            self.regex.is_match(input)
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            // Generate lots of test cases.
            cases: 1 << 14,
            .. ProptestConfig::default()
        })]

        #[test]
        // Test against a reference regex implementation.
        // Use a limited alphabet to increase the change of matches.
        fn test_matching_correctness(pattern in r"[ab%_]*", input in r"[ab]*") {
            let like_matcher = LikeMatcher::new(&pattern);
            let regex_matcher = RegexLikeMatcher::new(&pattern);
            let like_result = like_matcher.matches(&input);
            let regex_result = regex_matcher.matches(&input);
            prop_assert_eq!(like_result, regex_result);
        }

        #[test]
        // The first pattern can be read as "A string containing a special character".
        fn test_matching_never_fails_special(pattern in r".*[%_\\].*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The first pattern can be read as "A string containing 2 consecutive special characters".
        fn test_matching_never_fails_consecutive_special(pattern in r".*[%_\\]{2}.*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }

        #[test]
        // The pattern can be read as "A string not containing any special characters".
        fn test_literals_always_match(input in r"[^%_\\]*") {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
