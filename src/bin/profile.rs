use clap::{Parser, ValueEnum};
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
}

fn main() {
    let args = Args::parse();
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
