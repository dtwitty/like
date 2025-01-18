use std::ops::Deref;

/// A unit of a LIKE pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    Literal(&'a str),
    Any,
    Single,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tokens<'a>(Vec<Token<'a>>);

impl<'a> Tokens<'a> {
    pub fn from_str(s: &'a str) -> Self {
        Self(lex(s).0)
    }
}

impl<'a> Deref for Tokens<'a> {
    type Target = [Token<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> FromIterator<Token<'a>> for Tokens<'a> {
    fn from_iter<T: IntoIterator<Item = Token<'a>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
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
    let mut chars = i.chars();

    // Check special chars.
    // The unwrap is safe because we know the string is not empty.
    let c0 = chars.next().unwrap();
    if c0 == '%' {
        return (Token::Any, &i[1..]);
    }
    if c0 == '_' {
        return (Token::Single, &i[1..]);
    }

    // Check escapes.
    if c0 == '\\' {
        let c1 = chars.next();

        if c1.is_none() {
            return (Token::Literal("\\"), &i[1..]);
        }

        let c1 = c1.unwrap();

        if c1 == '%' {
            return (Token::Literal("%"), &i[2..]);
        }

        if c1 == '_' {
            return (Token::Literal("_"), &i[2..]);
        }

        if c1 == '\\' {
            return (Token::Literal("\\"), &i[2..]);
        }

        return (Token::Literal("\\"), &i[1..]);
    }

    // At this point, we have an unescaped literal.
    let pos = memchr::memchr3(b'%', b'_', b'\\', i.as_bytes());
    let pos = pos.unwrap_or(i.len());
    (Token::Literal(&i[..pos]), &i[pos..])
}
