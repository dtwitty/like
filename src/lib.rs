mod matchers;
mod patterns;
pub mod tokens;

use crate::matchers::{Matcher, Matchers, TerminalMatcher};
use crate::patterns::*;
use crate::tokens::*;

#[derive(Clone)]
enum PreFilter {
    EndsWith(String),
}

impl PreFilter {
    fn matches(&self, s: &str) -> bool {
        match self {
            PreFilter::EndsWith(suffix) => s.ends_with(suffix),
        }
    }
}

fn get_prefilters(matchers: &Matchers) -> Vec<PreFilter> {
    use Matcher::*;
    use TerminalMatcher::*;
    match &matchers[..] {
        // For these, we want there to be more than 1 matcher, otherwise we are just repeating work.
        [.., _, Terminal(EndsWith(s))] => vec![PreFilter::EndsWith(s.to_string())],
        [.., _, Terminal(Equals(s))] => vec![PreFilter::EndsWith(s.to_string())],
        _ => vec![],
    }
}

#[derive(Clone)]
pub struct LikeMatcher {
    prefilters: Vec<PreFilter>,
    matchers: Matchers,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let patterns = Patterns::from_tokens(tokens).optimize();
        let matchers = Matchers::from_patterns(patterns);
        let prefilters = get_prefilters(&matchers);
        LikeMatcher {
            prefilters,
            matchers,
        }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.prefilters.iter().all(|p| p.matches(input)) && self.matchers.matches(input)
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
        assert!(LikeMatcher::new("world").matches("world"));
        assert!(!LikeMatcher::new("hello").matches("world"));
    }

    #[test]
    fn test_starts_with() {
        assert!(LikeMatcher::new("hello%").matches("hello world"));
        assert!(!LikeMatcher::new("hello%").matches("world"));
    }

    #[test]
    fn test_ends_with() {
        assert!(LikeMatcher::new("%world").matches("hello world"));
        assert!(!LikeMatcher::new("%world").matches("hello"));
    }

    #[test]
    fn test_contains() {
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
        assert!(LikeMatcher::new("'%'").matches("' 'hello' world'"));
        assert!(LikeMatcher::new("a '%' b c").matches("a 'd' b c 'd' b c"));
        assert!(LikeMatcher::new("'%'%%'%'").matches("'a'a'a'a'a'a'a'a'a'"));
    }

    struct RegexLikeMatcher {
        regex: regex::Regex,
    }

    impl RegexLikeMatcher {
        fn new(pattern: &str) -> Self {
            let tokens = lex(pattern);
            let mut re = String::new();
            re.push('^');
            for token in tokens {
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
            cases: 1 << 18,
            .. ProptestConfig::default()
        })]

        #[test]
        fn test_matching_correctness(pattern in ".*", input in ".*") {
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
