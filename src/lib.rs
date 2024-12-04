use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::{map, value};
use nom::error::ErrorKind;
use nom::multi::many0;
use regex::Regex;

#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

fn lex(s: &str) -> Vec<Token> {
    many0(alt((
        value(Token::Any, tag::<_, _, (_, ErrorKind)>("%")),
        value(Token::Single, tag("_")),
        value(Token::Literal("\\"), tag("\\\\")),
        value(Token::Literal("%"), tag("\\%")),
        value(Token::Literal("_"), tag("\\_")),
        map(take_till1(|c| c == '%' || c == '_' || c == '\\'), |t| {
            Token::Literal(t)
        }),
    )))(s)
    .unwrap()
    .1
}

fn to_regex(tokens: Vec<Token>) -> Regex {
    let mut s = String::new();
    s.push('^');
    for t in tokens {
        match t {
            Token::Literal(l) => s.push_str(regex::escape(l).as_str()),
            Token::Any => s.push_str(".*"),
            Token::Single => s.push_str("."),
        }
    }
    s.push('$');
    Regex::new(&s).unwrap()
}

#[derive(Debug)]
struct LikeMatcher {
    regex: Regex,
}

impl LikeMatcher {
    pub fn new(s: &str) -> LikeMatcher {
        if s.is_empty() {
            return LikeMatcher {
                regex: Regex::new("^$").unwrap(),
            };
        }

        let tokens = lex(s);
        let regex = to_regex(tokens);

        LikeMatcher { regex }
    }

    pub fn matches(&self, s: &str) -> bool {
        self.regex.is_match(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }

    #[test]
    fn test_single() {
        assert!(LikeMatcher::new("h_llo").matches("hello"));
        assert!(!LikeMatcher::new("h_llo").matches("world"));
    }

    #[test]
    fn test_escape() {
        assert!(LikeMatcher::new(r"hello\%").matches("hello%"));
        assert!(LikeMatcher::new(r"hello\_").matches("hello_"));
        assert!(!LikeMatcher::new(r"hello\%").matches("hello"));
        assert!(!LikeMatcher::new(r"hello\_").matches("hello"));
    }
}
