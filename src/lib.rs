use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::{map, value};
use nom::error::ErrorKind;
use nom::multi::many0;

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
                return match self.tokens[t] {
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
                        &input[s..] == literal
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
                    if let Some(x) = input[s..].find(literal) {
                        // We found the literal. Skip over it and both tokens.
                        s += x + literal.len();
                        t += 2;
                    } else {
                        // We did not find the literal.
                        return false;
                    }
                }

                (Token::Single, _) => {
                    // Skip over the single character.
                    // If there isn't one, the loop condition will catch it.
                    s += 1;
                    t += 1;
                }

                (Token::Literal(literal), _) => {
                    if input[s..].starts_with(literal) {
                        // We found the literal. Skip over it.
                        s += literal.len();
                        t += 1;
                    } else {
                        // We did not find the literal.
                        return false;
                    }
                }
            }
        }

        let tokens_done = t == self.tokens.len();
        let input_done = s == input.len();
        tokens_done && input_done
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
