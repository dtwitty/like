use memchr::memmem::Finder;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::combinator::{map, value};
use nom::error::{context, ErrorKind};
use nom::multi::many0;

#[derive(Debug, Clone)]
enum Token<'a> {
    Literal(Finder<'a>),
    Any,
    Single,
}

fn lex(s: &[u8]) -> Vec<Token> {
    // Handle special characters and escapes.
    let any = value(Token::Any, tag::<_, _, (_, ErrorKind)>(b"%"));
    let single = value(Token::Single, tag(b"_"));
    let escaped_slash = value(Token::Literal(Finder::new(b"\\")), tag(b"\\\\"));
    let escaped_any = value(Token::Literal(Finder::new(b"%")), tag(b"\\%"));
    let escaped_single = value(Token::Literal(Finder::new(b"_")), tag(b"\\_"));
    let lone_escape = map(tag(b"\\"), |_| Token::Literal(Finder::new(b"\\")));

    // Handle literals.
    let until_special = take_till1(|c| c == b'%' || c == b'_' || c == b'\\');
    let literal = map(until_special, |t| Token::Literal(Finder::new(t)));

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

fn normalize(tokens: &mut Vec<Token>) {
    if tokens.is_empty() {
        return;
    }

    // We just want to fix the case of "%_", transforming it into "_%".
    // This way, we can just skip a single character instead of doing a complicated match.
    for i in 0..tokens.len() - 1 {
        if let (Token::Any, Token::Single) = (&tokens[i], &tokens[i + 1]) {
            tokens[i] = Token::Single;
            tokens[i + 1] = Token::Any;
        }
    }
}

#[derive(Debug, Clone)]
struct LikeMatcher<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> LikeMatcher<'a> {
    pub fn new(s: &[u8]) -> LikeMatcher {
        let mut tokens = lex(s);
        normalize(&mut tokens);
        LikeMatcher { tokens }
    }

    pub fn matches(&self, input: &[u8]) -> bool {
        if self.tokens.is_empty() {
            return input.is_empty();
        }

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
                        s + 1 == input.len()
                    }

                    Token::Literal(literal) => {
                        // Check whether the remaining input matches the literal.
                        &input[s..] == literal.needle()
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
                    if let Some(x) = literal.find(&input[s..]) {
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
                    if !(input[s..].starts_with(needle)) {
                        // We did not find the literal.
                        return false;
                    }

                    // We found the literal. Skip over it.
                    s += needle.len();
                    t += 1;
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::string::bytes_regex;

    #[test]
    fn test_empty() {
        assert!(LikeMatcher::new(b"").matches(b""));
        assert!(!LikeMatcher::new(b"").matches(b"world"));
    }

    #[test]
    fn test_literal() {
        assert!(LikeMatcher::new(b"world").matches(b"world"));
        assert!(!LikeMatcher::new(b"hello").matches(b"world"));
    }

    #[test]
    fn test_starts_with() {
        assert!(LikeMatcher::new(b"hello%").matches(b"hello world"));
        assert!(!LikeMatcher::new(b"hello%").matches(b"world"));
    }

    #[test]
    fn test_ends_with() {
        assert!(LikeMatcher::new(b"%world").matches(b"hello world"));
        assert!(!LikeMatcher::new(b"%world").matches(b"hello"));
    }

    #[test]
    fn test_contains() {
        assert!(LikeMatcher::new(b"%world%").matches(b"hello world"));
        assert!(LikeMatcher::new(b"%hello%").matches(b"hello world"));
        assert!(LikeMatcher::new(b"%llo wo%").matches(b"hello world"));
        assert!(!LikeMatcher::new(b"%world%").matches(b"hello"));
    }

    #[test]
    fn test_single() {
        assert!(LikeMatcher::new(b"_").matches(b"w"));
        assert!(!LikeMatcher::new(b"_").matches(b""));
        assert!(!LikeMatcher::new(b"_").matches(b"he"));
        assert!(LikeMatcher::new(b"h_llo").matches(b"hello"));
        assert!(!LikeMatcher::new(b"h_llo").matches(b"world"));
    }

    #[test]
    fn test_any() {
        assert!(LikeMatcher::new(b"%").matches(b"hello world"));
        assert!(LikeMatcher::new(b"%%").matches(b"hello world"));
        assert!(LikeMatcher::new(b"%").matches(b""));
        assert!(LikeMatcher::new(b"%%").matches(b""));
    }

    #[test]
    fn test_any_single() {
        assert!(!LikeMatcher::new(b"%_").matches(b""));
        assert!(!LikeMatcher::new(b"_%").matches(b""));
        assert!(LikeMatcher::new(b"%_").matches(b"hello world"));
        assert!(LikeMatcher::new(b"_%").matches(b"hello world"));
        assert!(LikeMatcher::new(b"%_").matches(b"h"));
        assert!(LikeMatcher::new(b"_%").matches(b"h"));
        assert!(LikeMatcher::new(b"h_%o").matches(b"hello"));
        assert!(LikeMatcher::new(b"h%_o").matches(b"hello"));
        assert!(LikeMatcher::new(b"h_%o").matches(b"hlo"));
        assert!(LikeMatcher::new(b"h%_o").matches(b"hlo"));
        assert!(!LikeMatcher::new(b"h_%o").matches(b"ho"));
        assert!(!LikeMatcher::new(b"h%_o").matches(b"ho"));
        assert!(!LikeMatcher::new(b"h_%o").matches(b"world"));
        assert!(!LikeMatcher::new(b"h%_o").matches(b"world"));
    }

    #[test]
    fn test_escape() {
        assert!(LikeMatcher::new(br"hello\%").matches(b"hello%"));
        assert!(LikeMatcher::new(br"hello\_").matches(b"hello_"));
        assert!(!LikeMatcher::new(br"hello\%").matches(b"hello"));
        assert!(!LikeMatcher::new(br"hello\_").matches(b"hello"));
        assert!(LikeMatcher::new(br"hel\\o%").matches(b"hel\\o"));
        assert!(!LikeMatcher::new(br"hel\o%").matches(b"hel\\p"));
        assert!(!LikeMatcher::new(br"hel\o%").matches(b"hel\\"));
        assert!(!LikeMatcher::new(br"hel\o%").matches(b"hl\\o"));
        assert!(LikeMatcher::new(br"h\%o").matches(b"h%o"));
    }

    #[test]
    fn test_wut() {
        LikeMatcher::new(b"\\h");
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1 << 14))]

        #[test]
        fn test_matching_never_fails(pattern in bytes_regex(".*").unwrap(), input in bytes_regex(".*").unwrap()) {
            let matcher = LikeMatcher::new(&pattern);
            matcher.matches(&input);
        }


        #[test]
        fn test_literals_always_match(input in bytes_regex("[^%_\\\\]*").unwrap()) {
            let matcher = LikeMatcher::new(&input);
            assert!(matcher.matches(&input));
        }
    }
}
