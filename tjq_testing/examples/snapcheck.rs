//! Snapshot / differential testing against real jq programs, e.g. jq's own
//! test suite (`tests/jq.test`). Each block is `program`, `input`, then N
//! expected-output lines (blank-separated); we run the program through tjq's
//! interpreter and categorize the outcome against the expected outputs. This
//! complements fuzzing: real programs exercise constructs and builtins the
//! generator does not reach. Usage: snapcheck <jq.test> [--show N]
use std::collections::HashMap;
use std::panic::{catch_unwind, AssertUnwindSafe};
use tjq_exec::{builtin_filters, parse, Filter, Json};

#[derive(Default)]
struct Tally {
    total: usize,
    pass: usize,
    diverge: usize,
    parse_panic: usize,
    exec_panic: usize,
    tjq_errored: usize, // tjq raised where jq produced output
    skipped: usize,     // %%FAIL / features we intentionally don't snapshot here
}

fn main() {
    let path = std::env::args().nth(1).expect("usage: snapcheck <jq.test>");
    let show: usize = std::env::args()
        .position(|a| a == "--show")
        .and_then(|i| std::env::args().nth(i + 1))
        .and_then(|s| s.parse().ok())
        .unwrap_or(25);
    let text = std::fs::read_to_string(&path).expect("read test file");

    let builtins = builtin_filters();
    let mut t = Tally::default();
    let mut shown = 0usize;

    for block in blocks(&text) {
        if block.len() < 2 {
            continue;
        }
        let program = block[0].trim();
        let input_s = block[1].trim();
        let expected_s = &block[2..];
        // Skip compile-failure tests and module/IO features out of scope here.
        if program.starts_with("%%FAIL")
            || program.contains("import ")
            || program.contains("include ")
            || program.contains("$__loc__")
            || program.contains("input")
            || program.contains("$ENV")
        {
            t.skipped += 1;
            continue;
        }
        t.total += 1;

        let input = match tjq_testing::parse_json(input_s) {
            Some(j) => j,
            None => {
                t.skipped += 1;
                t.total -= 1;
                continue;
            }
        };
        let expected: Option<Vec<Json>> = expected_s
            .iter()
            .map(|l| tjq_testing::parse_json(l.trim()))
            .collect();
        let expected = match expected {
            Some(e) => e,
            None => {
                t.skipped += 1;
                t.total -= 1;
                continue;
            }
        };

        let program_owned = program.to_string();
        let outcome = catch_unwind(AssertUnwindSafe(|| {
            let (defs, cst) = parse(&program_owned);
            let filter: Filter = (&cst).into();
            let mut scoped = builtins.clone();
            for (name, d) in defs {
                scoped.insert(name, (&d).into());
            }
            let mut ctx: HashMap<String, Filter> = HashMap::new();
            Filter::filter(&input, &filter, &scoped, &mut ctx)
        }));

        match outcome {
            Err(_) => {
                t.parse_panic += 1;
                report(&mut shown, show, "PANIC", program, input_s, &expected, None);
            }
            Ok(results) => {
                let mut oks = Vec::new();
                let mut had_err = false;
                for r in &results {
                    match r {
                        Ok(v) => oks.push(v.clone()),
                        Err(_) => had_err = true,
                    }
                }
                if had_err && oks.is_empty() {
                    t.tjq_errored += 1;
                    report(&mut shown, show, "ERROR", program, input_s, &expected, Some(&oks));
                } else if oks.len() == expected.len()
                    && oks.iter().zip(&expected).all(|(a, b)| tjq_testing::json_equal(a, b))
                {
                    t.pass += 1;
                } else {
                    t.diverge += 1;
                    report(&mut shown, show, "DIVERGE", program, input_s, &expected, Some(&oks));
                }
            }
        }
    }

    println!("\n=== snapcheck: {} ===", path);
    println!("considered:   {}", t.total);
    println!("  pass:       {}", t.pass);
    println!("  diverge:    {}", t.diverge);
    println!("  tjq errored:{}", t.tjq_errored);
    println!("  parse panic:{}", t.parse_panic);
    println!("  exec panic: {}", t.exec_panic);
    println!("skipped:      {}", t.skipped);
    let denom = t.total.max(1);
    println!(
        "pass rate (of considered): {:.1}%",
        100.0 * t.pass as f64 / denom as f64
    );
}

fn report(
    shown: &mut usize,
    limit: usize,
    kind: &str,
    prog: &str,
    input: &str,
    expected: &[Json],
    got: Option<&[Json]>,
) {
    if *shown >= limit {
        return;
    }
    *shown += 1;
    let show = |vs: &[Json]| {
        vs.iter()
            .map(|v| v.to_compact_string())
            .collect::<Vec<_>>()
            .join(" ")
    };
    println!("[{kind}] {prog}   | in: {input}");
    println!("   expected: [{}]", show(expected));
    if let Some(g) = got {
        println!("   tjq:      [{}]", show(g));
    }
}

/// Split the file into test blocks (comment lines dropped; blocks separated by
/// blank lines).
fn blocks(text: &str) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    let mut cur: Vec<String> = Vec::new();
    for line in text.lines() {
        if line.trim_start().starts_with('#') {
            continue;
        }
        if line.trim().is_empty() {
            if !cur.is_empty() {
                out.push(std::mem::take(&mut cur));
            }
        } else {
            cur.push(line.to_string());
        }
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}
