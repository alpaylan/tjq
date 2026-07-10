use std::collections::HashMap;
use std::io::Read;
use tjq_exec::{builtin_filters, parse, Filter, Json};

fn main() {
    let src = std::env::args()
        .nth(1)
        .expect("usage: interpprobe '<program>' (JSON input on stdin, default null)");
    // Read the input value from stdin (default null if empty).
    let mut buf = String::new();
    let _ = std::io::stdin().read_to_string(&mut buf);
    let input = tjq_testing::parse_json(buf.trim()).unwrap_or(Json::Null);
    let (_, cst) = parse(&src);
    let filter: Filter = (&cst).into();
    let builtins = builtin_filters();
    let mut var_ctx = HashMap::new();
    let results = Filter::filter(&input, &filter, &builtins, &mut var_ctx);
    for r in results {
        match r {
            Ok(v) => println!("ok: {}", v.to_compact_string()),
            Err(e) => println!("err: {e:?}"),
        }
    }
}
