use clap::{Parser, ValueEnum};
use like::{matchers::Matchers, patterns::Patterns, tokens::Tokens};
use std::hint::black_box;

#[derive(Debug, Clone, ValueEnum)]
enum Workload {
    Construct,
    Match,
    Full,
}

#[derive(Parser, Debug, Clone)]
struct Args {
    #[arg(short, long)]
    pattern: String,

    #[arg(short, long)]
    input: String,

    #[arg(short, long, default_value = "match")]
    workload: Workload,

    #[arg(long)]
    show_steps: bool,

    #[arg(long, default_value = "false")]
    skip_run: bool,
}

fn main() {
    let args = Args::parse();

    if args.show_steps {
        println!("Pattern: {:?}", args.pattern);
        println!("Input: {:?}", args.input);
        let tokens = Tokens::from_str(&args.pattern);
        println!("Tokens: {}", tokens);
        let patterns = Patterns::from_tokens(tokens).optimize();
        println!("Patterns: {}", patterns);
        let matchers = Matchers::from_patterns(patterns);
        println!("Matchers: {}", matchers);
    }

    if args.skip_run {
        return;
    }

    match args.workload {
        Workload::Construct => loop {
            let _ = black_box(like::LikeMatcher::new(&args.pattern));
        },

        Workload::Match => {
            let matcher = like::LikeMatcher::new(&args.pattern);

            loop {
                let _ = black_box(matcher.matches(&args.input));
            }
        }

        Workload::Full => loop {
            let matcher = black_box(like::LikeMatcher::new(&args.pattern));
            let _ = black_box(matcher.matches(&args.input));
        },
    }
}
