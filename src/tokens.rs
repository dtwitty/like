use std::slice::Iter;
use std::vec::IntoIter;

/// A unit of a LIKE pattern.
#[derive(Debug, Clone)]
pub enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

#[derive(Debug)]
pub struct Tokens<'a> {
    tokens: Vec<Token<'a>>,
}

pub type TokensIntoIter<'a> = IntoIter<Token<'a>>;
impl<'a> IntoIterator for Tokens<'a> {
    type Item = Token<'a>;
    type IntoIter = TokensIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

type TokensIter<'a> = Iter<'a, Token<'a>>;

impl<'a> IntoIterator for &'a Tokens<'a> {
    type Item = &'a Token<'a>;
    type IntoIter = TokensIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.iter()
    }
}

/// Lexes a LIKE pattern into tokens. Never fails because all strings are valid patterns.
pub fn lex(input: &str) -> Tokens {
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        let (t, rest) = lex_one(s);
        tokens.push(t);
        s = rest;
    }

    Tokens { tokens }
}

/// Lexes a single token from the input. Never fails because all strings are valid patterns.
fn lex_one(i: &str) -> (Token, &str) {
    match i {
        s if s.starts_with('%') => (Token::Any, &s[1..]),
        s if s.starts_with('_') => (Token::Single, &s[1..]),
        s if s.starts_with("\\\\") => (Token::Literal("\\"), &s[2..]),
        s if s.starts_with("\\%") => (Token::Literal("%"), &s[2..]),
        s if s.starts_with("\\_") => (Token::Literal("_"), &s[2..]),
        s if s.starts_with('\\') => (Token::Literal("\\"), &s[1..]),
        _ => {
            let pos = memchr::memchr3(b'%', b'_', b'\\', i.as_bytes());
            let pos = pos.unwrap_or(i.len());
            (Token::Literal(&i[..pos]), &i[pos..])
        }
    }
}
