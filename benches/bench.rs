#![allow(unused)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use like::tokens::Token::*;
use like::{tokens::Tokens, LikeMatcher};
use std::collections::BTreeSet;

struct RegexLikeMatcher {
    regex: regex::Regex,
}

impl RegexLikeMatcher {
    fn new(pattern: &str) -> Self {
        let tokens = Tokens::from_str(pattern);
        let mut re = String::new();
        re.push('^');
        for token in &tokens[..] {
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
        (
            "%1%22%333%4444%",
            "a super long text that contains an example of the given pattern, which starts with a \
            1, then is followed by a 22, later is continued with a 3333, and finally a 4444 before \
            continuing on for some arbitrarily long number of characters.",
        ),
        (
            "%1%22%333%4444%",
            "a super long text that does not match the given pattern, which starts with a 1, then \
            is followed by a 22, later is continued with a 3333, and but has only 444 before \
            continuing on for some arbitrarily long number of characters.",
        ),
        ("_x_", "this is a long string"),
        ("________", "aaa"),
        ("________", "aaaaaaaa"),
        ("________", "aaaaaaaaaaaaaaaa"),
        ("exact-match", "exact-match"),
        ("%a%", "abc"),
        ("_b_", "abc"),
        ("a_c", "abc"),
        ("x_y", "abc"),
        ("%hello%", "this is a hello world example"),
        ("_h_llo", "oh hello there"),
        ("%abc", "1234567890abcdefghijklmnopqrstuvwxyz"),
        ("a%very_long_pattern_that_does_not_match", "short"),
        ("%short_text", "short_text"),
        ("long_pattern_that_ends_in_a%z", "long_end_in_a_z"),
        ("lo%rld", "loworld"),
        ("%this%is%long%", "this text is very long and matches"),
        (
            "%pattern_not_found%",
            "this string does not contain the pattern",
        ),
        ("l%o%r%g", "lots_of_random_gibberish"),
        ("start%middle%end", "start_some_content_middle_end"),
        ("", ""),
        ("%", ""),
        ("_", ""),
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

        /*
        group.bench_function(format!("RegexLikeMatcher(\"{pattern}\")"), |b| {
            b.iter(|| RegexLikeMatcher::new(black_box(pattern)))
        });
         */
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

        let wild_like_matcher = wildmatch::WildMatchPattern::<'%', '_'>::new(pattern);
        group.bench_function(
            format!("WildMatchPattern(\"{pattern}\").matches(\"{input}\")"),
            |b| b.iter(|| wild_like_matcher.matches(black_box(input))),
        );

        let char_pattern = pattern.chars().collect::<Vec<_>>();
        let wildcard_matcher = wildcard::WildcardBuilder::new(&char_pattern)
            .with_any_metasymbol('%')
            .with_one_metasymbol('_')
            .build()
            .unwrap();
        let char_slice = input.chars().collect::<Vec<_>>();
        group.bench_function(
            format!("WildcardMatcher(\"{pattern}\").matches(\"{input}\")"),
            |b| b.iter(|| wildcard_matcher.is_match(black_box(&char_slice))),
        );
        */
    }

    group.finish();
}

criterion_group!(construction, benchmark_construction);
criterion_group!(matching, benchmark_matching);
criterion_main!(matching);
