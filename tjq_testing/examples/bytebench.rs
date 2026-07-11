//! Micro-benchmark: bytecode VM vs the tree-walking interpreter on core
//! programs. Establishes a baseline for the type-directed-optimization work
//! (and shows where the current VM's cost sits — notably full stack snapshots
//! on every fork). Not a jq comparison (that needs process-overhead control);
//! run `jq` yourself on the printed programs for a rough external point.
use std::collections::HashMap;
use std::time::Instant;
use tjq_exec::bytecode::{compile, run};
use tjq_exec::{builtin_filters, Filter, Json};

fn parse_filter(src: &str) -> Filter {
    let (_defs, cst) = tjq_exec::parse(src);
    (&cst).into()
}

fn big_array(n: usize) -> Json {
    Json::Array((0..n).map(|i| Json::Number(i as f64)).collect())
}

fn main() {
    let reps: usize = std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(2000);
    let n: usize = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(1000);
    let input = big_array(n);
    let builtins = builtin_filters();

    let progs = [
        ".[] | . + 1",
        ".[] | (. * 2) + 1",
        "[.[] | . + 1]",
        ".[] | if . % 2 == 0 then . else -. end",
        ".[] | {v: (. + 1)}",
    ];

    println!(
        "reps={reps} input=[0..{n}]\n{:<40} {:>12} {:>12} {:>8}",
        "program", "vm (ms)", "tree (ms)", "vm/tree"
    );
    for src in progs {
        let filter = parse_filter(src);
        let code = match compile(&filter) {
            Ok(c) => c,
            Err(_) => {
                println!("{src:<40} (unsupported)");
                continue;
            }
        };

        // Warm up and sanity-check both engines agree on output length.
        let vm_n = run(&code, input.clone()).len();
        let mut ctx: HashMap<String, Filter> = HashMap::new();
        let tree_n = Filter::filter(&input, &filter, &builtins, &mut ctx).len();
        assert_eq!(vm_n, tree_n, "length mismatch for {src}");

        let t0 = Instant::now();
        for _ in 0..reps {
            std::hint::black_box(run(&code, input.clone()));
        }
        let vm_ms = t0.elapsed().as_secs_f64() * 1e3;

        let t1 = Instant::now();
        for _ in 0..reps {
            let mut ctx: HashMap<String, Filter> = HashMap::new();
            std::hint::black_box(Filter::filter(&input, &filter, &builtins, &mut ctx));
        }
        let tree_ms = t1.elapsed().as_secs_f64() * 1e3;

        println!(
            "{src:<40} {vm_ms:>12.1} {tree_ms:>12.1} {:>8.2}",
            vm_ms / tree_ms
        );
    }
}
