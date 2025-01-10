#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use like::{lex, LikeMatcher};

struct RegexLikeMatcher {
    regex: regex::Regex,
}

impl RegexLikeMatcher {
    fn new(pattern: &str) -> Self {
        let tokens = lex(pattern);
        let mut re = String::new();
        re.push('^');
        for token in tokens {
            match token {
                like::Token::Any => re.push_str(".*"),
                like::Token::Single => re.push_str("."),
                like::Token::Literal(lit) => re.push_str(&regex::escape(lit)),
            }
        }
        re.push('$');
        Self {
            regex: regex::Regex::new(&re).unwrap(),
        }
    }

    fn matches(&self, input: &str) -> bool {
        self.regex.is_match(input)
    }
}

fn benchmark_matchers(c: &mut Criterion) {
    let pattern = "'%'%'";
    let haystack = "'a'a'a'a'a'a'a'a'a'";

    c.bench_function("LikeMatcher construction", |b| {
        b.iter(|| LikeMatcher::new(black_box(pattern)))
    });

    c.bench_function("RegexLikeMatcher construction", |b| {
        b.iter(|| RegexLikeMatcher::new(black_box(pattern)))
    });

    let like_matcher = LikeMatcher::new(pattern);
    c.bench_function("LikeMatcher Matching", |b| {
        b.iter(|| like_matcher.matches(black_box(haystack)))
    });

    let regex_like_matcher = RegexLikeMatcher::new(pattern);
    c.bench_function("RegexLikeMatcher Matching", |b| {
        b.iter(|| regex_like_matcher.matches(black_box(haystack)))
    });
}

criterion_group!(benches, benchmark_matchers);
criterion_main!(benches);
