#![allow(unused)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use like::tokens::Token::*;
use like::{tokens::lex, LikeMatcher};
use std::collections::BTreeSet;

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
                Any => re.push_str(".*"),
                Single => re.push_str("."),
                Literal(lit) => re.push_str(&regex::escape(lit)),
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

fn patterns_and_inputs() -> &'static [(&'static str, &'static str)] {
    &[
        ("", ""),
        ("", "world"),
        ("world", "world"),
        ("hello", "world"),
        ("hello%", "hello world"),
        ("hello%", "world"),
        ("%world", "hello world"),
        ("%world", "hello"),
        ("%world%", "hello world"),
        ("%hello%", "hello world"),
        ("%llo wo%", "hello world"),
        ("%world%", "hello"),
        ("%🔥%", "hello 🔥"),
        ("%🔥%", "🔥hello"),
        ("_", ""),
        ("_", "w"),
        ("_", "he"),
        ("_______________________", "aaaaaaaaaaaaaaaaaaaaaaa"),
        ("h_llo", "hello"),
        ("h_llo", "world"),
        ("h_llo", "h🔥llo"),
        ("%", "hello world"),
        ("%%", "hello world"),
        ("%", ""),
        ("%%", ""),
        ("h_%o", "ho"),
        ("%_", ""),
        ("_%", ""),
        ("%_", "hello world"),
        ("_%", "hello world"),
        ("%_", "h"),
        ("_%", "h"),
        ("h_%o", "hello"),
        ("h%_o", "hello"),
        ("h_%o", "hlo"),
        ("h%_o", "hlo"),
        ("h%_o", "ho"),
        ("h_%o", "world"),
        ("h%_o", "world"),
        (r"hello\%", "hello%"),
        (r"hello\_", "hello_"),
        (r"hello\%", "hello"),
        (r"hello\_", "hello"),
        (r"hel\\o%", "hel\\o"),
        (r"hel\o%", "hel\\p"),
        (r"hel\o%", "hel\\"),
        (r"hel\o%", "hl\\o"),
        (r"h\%o", "h%o"),
        ("'%'", "' 'hello' world'"),
        ("a '%' b c", "a 'd' b c 'd' b c"),
        ("'%'%'", "'a'a'a'a'a'a'a'a'a'"),
    ]
}

fn benchmark_construction(c: &mut Criterion) {
    let mut group = c.benchmark_group("Construction");

    let unique_patterns = patterns_and_inputs()
        .iter()
        .map(|(pattern, _)| pattern)
        .collect::<BTreeSet<_>>();
    for pattern in unique_patterns {
        group.bench_function(format!("LikeMatcher(\"{pattern}\")"), |b| {
            b.iter(|| LikeMatcher::new(black_box(pattern)))
        });

        group.bench_function(format!("RegexLikeMatcher(\"{pattern}\")"), |b| {
            b.iter(|| RegexLikeMatcher::new(black_box(pattern)))
        });
    }

    group.finish();
}

fn benchmark_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("Matching");

    for (pattern, input) in patterns_and_inputs() {
        let like_matcher = LikeMatcher::new(pattern);
        group.bench_function(
            format!("LikeMatcher(\"{pattern}\").matches(\"{input}\")"),
            |b| b.iter(|| like_matcher.matches(black_box(input))),
        );

        /*
        let regex_like_matcher = RegexLikeMatcher::new(pattern);
        group.bench_function(
            format!("RegexLikeMatcher(\"{pattern}\").matches(\"{input}\")"),
            |b| b.iter(|| regex_like_matcher.matches(black_box(input))),
        );
         */
    }

    group.finish();
}

//criterion_group!(construction, benchmark_construction);
criterion_group!(matching, benchmark_matching);
criterion_main!(matching);
