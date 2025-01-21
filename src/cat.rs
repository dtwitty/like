#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cat<'a> {
    v: Vec<&'a str>,
}

impl<'a> Cat<'a> {
    pub fn from_str(s: &'a str) -> Self {
        Self { v: vec![s] }
    }

    pub fn merge_with(&mut self, other: Self) {
        self.v.extend(other.v);
    }

    pub fn to_string(&self) -> String {
        self.v.join("")
    }
}
