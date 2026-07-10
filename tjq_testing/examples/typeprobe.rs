//! Print inferred tin/tout for a program given on the command line.
use tjq_exec::{builtin_filters, parse, Filter};
use tjq_semantics::experimental_type_inference::{
    compute_shape, solve, solve_arrows, Context, TypeOptions,
};

fn main() {
    let src = std::env::args()
        .nth(1)
        .expect("usage: typeprobe '<program>'");
    let (_, cst) = parse(&src);
    let filter: Filter = (&cst).into();
    let builtins = builtin_filters();
    let mut ctx = Context::new();
    ctx.options = TypeOptions {
        lenient_absence: true,
    };
    let i = ctx.fresh();
    let o = ctx.fresh();
    let constraints = compute_shape(&filter, &mut ctx, i, o, &builtins);
    // Arrow view: intersection of per-overload arrows (preserves the
    // input/output correlation).
    match solve_arrows(constraints.clone(), &ctx, i, o) {
        Ok(arrow) => println!("arrow: {}", arrow),
        Err(e) => println!("arrow solve error: {}", e.message),
    }
    match solve(constraints, &ctx) {
        Ok(res) => {
            println!("errors: {:?}", res.errors);
            // Union view: sound but loses the correlation.
            println!("tin:  {}", res.get(i).canonicalize());
            println!("tout: {}", res.get(o).canonicalize());
        }
        Err(e) => println!("solve error: {}", e.message),
    }
}
