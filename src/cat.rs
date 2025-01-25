/// A simple struct that holds a vector of string slices.
/// This allows us to build up a string out of parts, only allocating and copying when necessary.
/// The concatenation is always non-empty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cat<'a> {
    v: Vec<&'a str>,
}

impl<'a> Cat<'a> {
    pub fn from_str(s: &'a str) -> Self {
        if s.is_empty() {
            panic!("Cat::from_str called with empty string");
        }

        Self { v: vec![s] }
    }

    pub fn merge_with(mut self, mut other: Self) -> Self {
        self.v.extend(other.v.drain(..));
        self
    }

    pub fn to_string(&self) -> String {
        self.v.join("")
    }
}
