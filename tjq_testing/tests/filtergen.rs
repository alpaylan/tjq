//! The generated jq source must mean what the generator intended: printing
//! and re-parsing has to reach a fixpoint (print ∘ parse ∘ print = print).
//! AST equality is too strict (the parser may desugar), so we compare the
//! re-printed source instead.

use tjq_exec::{parse, Filter};
use tjq_testing::filtergen::{gen_filter, to_jq_source};
use tjq_testing::rng::Rng;

const CASES: usize = 5_000;

#[test]
fn printed_programs_reparse_to_fixpoint() {
    let mut rng = Rng::new(0xF117);
    for case in 0..CASES {
        let f = gen_filter(&mut rng, 3);
        let src = to_jq_source(&f);
        let (_, cst) = parse(&src);
        let reparsed: Filter = (&cst).into();
        let src2 = to_jq_source(&reparsed);
        assert_eq!(
            src, src2,
            "case {case}: print/parse fixpoint failed\n  ast: {f:?}\n  \
             printed:  {src}\n  reparsed: {reparsed:?}\n  reprinted: {src2}"
        );
    }
}
