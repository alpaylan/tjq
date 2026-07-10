use std::collections::HashMap;
use tjq_exec::{builtin_filters, parse, Filter, Json};

fn main() {
    let src = std::env::args()
        .nth(1)
        .expect("usage: interpprobe '<program>'");
    let (_, cst) = parse(&src);
    let filter: Filter = (&cst).into();
    let builtins = builtin_filters();
    let mut var_ctx = HashMap::new();
    let results = Filter::filter(&Json::Null, &filter, &builtins, &mut var_ctx);
    for r in results {
        match r {
            Ok(v) => println!("ok: {}", v.to_compact_string()),
            Err(e) => println!("err: {e:?}"),
        }
    }
}
