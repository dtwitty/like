use memchr::memmem::Finder;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::{map, value};
use nom::error::ErrorKind;
use nom::multi::many0;

#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(Finder<'a>),
    Any,
    Single,
}

fn lex(s: &str) -> Vec<Token> {
    many0(alt((
        value(Token::Any, tag::<_, _, (_, ErrorKind)>("%")),
        value(Token::Single, tag("_")),
        value(Token::Literal(Finder::new("\\")), tag("\\\\")),
        value(Token::Literal(Finder::new("%")), tag("\\%")),
        value(Token::Literal(Finder::new("_")), tag("\\_")),
        map(take_till1(|c| c == '%' || c == '_' || c == '\\'), |t| {
            Token::Literal(Finder::new(t))
        }),
    )))(s)
    .unwrap()
    .1
}

fn normalize(tokens: &mut Vec<Token>) {
    // We just want to fix the case of "%_", transforming it into "_%".
    // This way, we can just skip a single character instead of doing a complicated match.
    for i in 0..tokens.len() - 1 {
        if let (Token::Any, Token::Single) = (&tokens[i], &tokens[i + 1]) {
            tokens[i] = Token::Single;
            tokens[i + 1] = Token::Any;
        }
    }
}

#[derive(Debug)]
struct LikeMatcher<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> LikeMatcher<'a> {
    pub fn new(s: &str) -> LikeMatcher {
        let mut tokens = lex(s);
        normalize(&mut tokens);
        LikeMatcher { tokens }
    }

    pub fn matches(&self, input: &str) -> bool {
        // The index into the list of tokens.
        let mut t = 0;

        // The index into the string.
        let mut s = 0;

        while t < self.tokens.len() {
            if t == self.tokens.len() - 1 {
                // This is the last token.
                return match &self.tokens[t] {
                    Token::Any => {
                        // We can match anything that's left. We're done.
                        true
                    }

                    Token::Single => {
                        // We need to match a single character at the end.
                        // This is equivalent to saying "we have 1 character left".
                        s == input.len() - 1
                    }

                    Token::Literal(literal) => {
                        // Check whether the remaining input matches the literal.
                        input[s..].as_bytes() == literal.needle()
                    }
                };
            }

            // We have at least 2 tokens left.
            match (&self.tokens[t], &self.tokens[t + 1]) {
                (Token::Any, Token::Any) => {
                    // We can just skip the first Any token.
                    t += 1;
                }

                (Token::Any, Token::Single) => {
                    unreachable!("Normalization should have caught Any followed by Single.");
                }

                (Token::Any, Token::Literal(literal)) => {
                    // Skip to the next literal.
                    if let Some(x) = literal.find(&input[s..].as_bytes()) {
                        // We found the literal. Skip over it and both tokens.
                        let needle = literal.needle();
                        s += x + needle.len();
                        t += 2;
                    } else {
                        // We did not find the literal.
                        return false;
                    }
                }

                (Token::Single, _) => {
                    // Skip over the single character if we can.
                    if s == input.len() {
                        return false;
                    }

                    s += 1;
                    t += 1;
                }

                (Token::Literal(literal), _) => {
                    let needle = literal.needle();
                    if input[s..].as_bytes().starts_with(needle) {
                        // We found the literal. Skip over it.
                        s += needle.len();
                        t += 1;
                    } else {
                        // We did not find the literal.
                        return false;
                    }
                }
            }
        }

        true
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
    fn test_any() {
        assert!(LikeMatcher::new("%").matches("hello world"));
        assert!(LikeMatcher::new("%%").matches("hello world"));
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
    }
}
