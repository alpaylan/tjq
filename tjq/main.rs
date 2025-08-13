use std::collections::HashMap;

use clap::Parser;
use clap_derive::Parser;
use serde_json::Value;
use tjq_exec::{parse, parse_defs};
use tjq_exec::{Filter, Json};
use tjq_semantics::Shape;

#[derive(Parser)]
struct CLI {
    #[clap(long)]
    expression: Option<String>,
    #[clap(long)]
    path: Option<String>,
    #[clap(long)]
    input: Option<String>,
    #[clap(long)]
    input_path: Option<String>,
    #[clap(long)]
    verbose: bool,
}

pub fn builtin_filters() -> HashMap<String, Filter> {
    // read defs.jq

    parse_defs(include_str!("defs.jq"))
}

fn main() {
    tracing_subscriber::fmt()
        .with_target(false)
        .with_thread_ids(true)
        .with_thread_names(true)
        .with_file(true)
        .with_line_number(true)
        .init();

    let args = CLI::parse();
    let expression = args
        .expression
        .or_else(|| {
            if let Some(path) = &args.path {
                std::fs::read_to_string(path).ok()
            } else {
                None
            }
        })
        .expect("no expression provided, either as an argument or a file path");

    let (defs, filter) = parse(expression.as_str());

    let json = args
        .input
        .or_else(|| {
            if let Some(path) = &args.input_path {
                std::fs::read_to_string(path).ok()
            } else {
                None
            }
        })
        .expect("no input provided, either as an argument or a file path");
    let json =
        Json::from_serde_value(serde_json::from_str::<Value>(json.as_str()).expect("invalid JSON"));

    tracing::info!("input: '{}'", json);
    tracing::info!("filter: '{}'", filter);

    let mut filters = builtin_filters();
    filters.extend(defs);

    let results = Filter::filter(&json, &filter, &filters, &mut Default::default());

    println!("===========JQ Filter Results===========");
    for result in results {
        match result {
            Ok(result) => {
                println!("{result}");
            }
            Err(err) => {
                println!("{err}");
            }
        }
    }
    println!("=======================================");

    let (s, results) = Shape::new(&filter, &filters);

    println!("inferred input shape: {}", s);
    println!(
        "expected result shapes: {}",
        results
            .iter()
            .map(Shape::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    );
    // Check internal type mismatches in the shape
    println!("type checking jq expression...");
    let m = s.check_self(vec![]);
    match m {
        Some(m) => println!("{m}"),
        None => {
            println!("[✓] jq expression is well typed.");
            println!("checking input shape against the program...");
            let m = s.check(json, vec![]);

            match m {
                Some(m) => println!("{m}"),
                None => println!("[✓] the input conforms to the inferred shape."),
            }
        }
    }
}
