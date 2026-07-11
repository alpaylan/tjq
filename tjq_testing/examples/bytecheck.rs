//! Differential test: the bytecode VM vs the tree-walking interpreter.
//!
//! Generates random programs; for each that compiles to the bytecode core,
//! runs both engines on random inputs and compares the full output streams
//! (Ok values by JSON equality, errors kind-agnostically). Reports any
//! divergence with a reproducer. Usage: bytecheck [seed] [iters] [depth]
use std::collections::HashMap;
use tjq_exec::bytecode::{compile, run};
use tjq_exec::{builtin_filters, Filter, Json};
use tjq_testing::filtergen::{gen_filter, to_jq_source};
use tjq_testing::jsongen::gen_json;
use tjq_testing::rng::Rng;

/// Compare two output streams for equivalence, tolerating error *multiplicity*.
/// The tree interpreter is eager (it evaluates both binop operand streams fully
/// before the cartesian product) while the VM backtracks lazily like jq, so
/// when an operand errors they can emit a different *number* of errors. We
/// therefore compare the sequence of Ok values exactly (by JSON equality) and
/// require only that error *presence* agrees.
fn stream_eq(a: &[Result<Json, tjq_exec::JQError>], b: &[Result<Json, tjq_exec::JQError>]) -> bool {
    let oks = |s: &[Result<Json, tjq_exec::JQError>]| {
        s.iter().filter_map(|r| r.as_ref().ok().cloned()).collect::<Vec<_>>()
    };
    let has_err = |s: &[Result<Json, tjq_exec::JQError>]| s.iter().any(|r| r.is_err());
    let (ao, bo) = (oks(a), oks(b));
    ao.len() == bo.len()
        && ao.iter().zip(&bo).all(|(x, y)| tjq_testing::json_equal(x, y))
        && has_err(a) == has_err(b)
}

fn main() {
    let seed: u64 = std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(1);
    let iters: u64 = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(20000);
    let depth: usize = std::env::args().nth(3).and_then(|s| s.parse().ok()).unwrap_or(5);

    let builtins = builtin_filters();
    let mut compiled = 0u64;
    let mut checked = 0u64;
    let mut diverged = 0u64;

    for i in 0..iters {
        let mut rng = Rng::new(seed.wrapping_add(i));
        let filter = gen_filter(&mut rng, depth);
        let code = match compile(&filter) {
            Ok(c) => c,
            Err(_) => continue, // outside the supported core
        };
        compiled += 1;

        let mut irng = Rng::new(seed.wrapping_add(i).wrapping_mul(2654435761));
        for _ in 0..12 {
            let input = gen_json(&mut irng, 3);
            let vm_out = run(&code, input.clone());
            let mut ctx: HashMap<String, Filter> = HashMap::new();
            let tree_out = Filter::filter(&input, &filter, &builtins, &mut ctx);
            checked += 1;
            if !stream_eq(&vm_out, &tree_out) {
                diverged += 1;
                if diverged <= 20 {
                    println!("DIVERGE prog: {}", to_jq_source(&filter));
                    println!("  input: {}", input.to_compact_string());
                    let show = |s: &[Result<Json, tjq_exec::JQError>]| {
                        s.iter()
                            .map(|r| match r {
                                Ok(j) => j.to_compact_string(),
                                Err(_) => "ERR".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    };
                    println!("  vm:   [{}]", show(&vm_out));
                    println!("  tree: [{}]", show(&tree_out));
                }
            }
        }
    }

    println!(
        "\nbytecheck seed={seed} iters={iters}: compiled={compiled} checks={checked} diverged={diverged}"
    );
    if diverged > 0 {
        std::process::exit(1);
    }
}
