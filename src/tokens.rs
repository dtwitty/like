use std::fmt::Display;
use std::ops::Deref;

/// A unit of a LIKE pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(s) => write!(f, "Literal(\"{}\")", s),
            Token::Any => write!(f, "Any"),
            Token::Single => write!(f, "Single"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tokens<'a>(Vec<Token<'a>>);

impl<'a> Tokens<'a> {
    pub fn from_str(s: &'a str) -> Self {
        Self(lex(s).0)
    }
}

impl Display for Tokens<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Deref for Tokens<'a> {
    type Target = [Token<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Lexes a LIKE pattern into tokens. Never fails because all strings are valid patterns.
fn lex(input: &str) -> Tokens {
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        let (t, rest) = lex_one(s);
        tokens.push(t);
        s = rest;
    }

    Tokens(tokens)
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
