//! A minimal tjq runner for fair benchmarking against jq/jaq/gojq: reads a JSON
//! input file, compiles the program to bytecode, runs it, and serializes each
//! output on its own line (jq's default). Usage: tjqrun '<program>' <input.json>
use std::io::{BufWriter, Write};
use tjq_exec::bytecode::{compile, run};
use tjq_exec::Json;

fn main() {
    // Accept (and ignore) jq-style flags like `-c` so the same command line
    // benchmarks all four tools uniformly.
    let args: Vec<String> = std::env::args()
        .skip(1)
        .filter(|a| !a.starts_with('-'))
        .collect();
    let prog = args.first().expect("usage: tjqrun [-c] '<prog>' <input.json>").clone();
    let path = args.get(1).expect("usage: tjqrun [-c] '<prog>' <input.json>").clone();
    let text = std::fs::read_to_string(&path).expect("read input");
    let input = tjq_testing::parse_json(text.trim()).expect("parse input");

    let (_defs, cst) = tjq_exec::parse(&prog);
    let filter: tjq_exec::Filter = (&cst).into();
    let code = compile(&filter).expect("program uses an unsupported construct");

    let outputs = run(&code, input);
    let stdout = std::io::stdout();
    let mut w = BufWriter::new(stdout.lock());
    for r in &outputs {
        match r {
            Ok(v) => {
                let _ = writeln!(w, "{}", v.to_compact_string());
            }
            Err(_) => {
                let _ = writeln!(w, "error");
            }
        }
    }
    let _ = w.flush();
    // Keep the value alive so the compiler cannot elide the work.
    std::hint::black_box(&outputs as *const Vec<Result<Json, _>>);
}
