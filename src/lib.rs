use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::{map, value};
use nom::error::{context, ErrorKind};
use nom::multi::many0;
use regex::Regex;

#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

fn lex(s: &str) -> Vec<Token> {
    // Handle special characters and escapes.
    let any = value(Token::Any, tag::<_, _, (_, ErrorKind)>("%"));
    let single = value(Token::Single, tag("_"));
    let escaped_slash = value(Token::Literal("\\"), tag("\\\\"));
    let escaped_any = value(Token::Literal("%"), tag("\\%"));
    let escaped_single = value(Token::Literal("_"), tag("\\_"));
    let lone_escape = map(tag("\\"), |_| Token::Literal("\\"));

    // Handle literals.
    let until_special = take_till1(|c| c == '%' || c == '_' || c == '\\');
    let literal = map(until_special, |t| Token::Literal(t));

    // Combine all the parsers for a single token.
    let single_token = context(
        "single_token",
        alt((
            any,
            single,
            escaped_slash,
            escaped_any,
            escaped_single,
            lone_escape,
            literal,
        )),
    );

    // Use the many0 combinator to parse multiple single tokens.
    let mut all = many0(single_token);
    let parsed = all(s);
    parsed.unwrap().1
}

#[derive(Debug, Clone)]
pub struct LikeMatcher {
    re: Regex,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        let tokens = lex(s);
        let mut regex = String::new();
        regex.push('^');

        for t in tokens {
            match t {
                Token::Literal(l) => regex.push_str(&regex::escape(l)),
                Token::Any => regex.push_str(".*"),
                Token::Single => regex.push_str("."),
            }
        }

        regex.push('$');

        let re = Regex::new(&regex).unwrap();
        LikeMatcher { re }
    }

    pub fn matches(&self, input: &str) -> bool {
        self.re.is_match(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert!(LikeMatcher::new("_").matches("w"));
        assert!(!LikeMatcher::new("_").matches(""));
        assert!(!LikeMatcher::new("_").matches("he"));
        assert!(LikeMatcher::new("h_llo").matches("hello"));
        assert!(!LikeMatcher::new("h_llo").matches("world"));
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
        assert!(!LikeMatcher::new("h_%o").matches("ho"));
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
    fn test_greedy() {
        // This is a regression test for a bug where the '%' would match as little as possible.
        // The '%' should match as much as possible.
        assert!(LikeMatcher::new("a '%' b c").matches("a 'd' b c 'd' b c"));
        assert!(LikeMatcher::new("'%'").matches("' 'hello' world'"));
    }

    proptest! {
        // Generate lots of test cases.
        #![proptest_config(ProptestConfig::with_cases(1 << 14))]

        #[test]
        fn test_matching_never_fails(pattern in ".*", input in ".*") {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }


        #[test]
        fn test_literals_always_match(input in "[^%_\\\\]*") {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
