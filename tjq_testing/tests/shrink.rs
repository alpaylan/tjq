//! Shrinker properties: the result still fails, and gets small.

use tjq_exec::{BinOp, Filter, Json};
use tjq_testing::jsongen::{gen_json_profiled, SizeProfile};
use tjq_testing::rng::Rng;
use tjq_testing::shrink::shrink_case;

fn json_nodes(j: &Json) -> usize {
    1 + match j {
        Json::Array(a) => a.iter().map(json_nodes).sum(),
        Json::Object(o) => o.iter().map(|(_, v)| json_nodes(v)).sum(),
        _ => 0,
    }
}

fn contains_number(j: &Json) -> bool {
    match j {
        Json::Number(_) => true,
        Json::Array(a) => a.iter().any(contains_number),
        Json::Object(o) => o.iter().any(|(_, v)| contains_number(v)),
        _ => false,
    }
}

fn contains_mod(f: &Filter) -> bool {
    match f {
        Filter::BinOp(l, BinOp::Mod, r) => {
            true || contains_mod(l) || contains_mod(r) // op itself suffices
        }
        Filter::BinOp(l, _, r) | Filter::Pipe(l, r) | Filter::Comma(l, r) => {
            contains_mod(l) || contains_mod(r)
        }
        Filter::UnOp(_, x) | Filter::ObjIndex(x) | Filter::ArrayIndex(x) => contains_mod(x),
        Filter::IfThenElse(c, t, e) => contains_mod(c) || contains_mod(t) || contains_mod(e),
        Filter::Array(items) => items.iter().any(contains_mod),
        Filter::Object(items) => items.iter().any(|(_, v)| contains_mod(v)),
        _ => false,
    }
}

/// Shrinking a large input against a structural predicate reaches a small
/// witness that still satisfies the predicate.
#[test]
fn shrinks_large_inputs_to_small_witnesses() {
    let mut rng = Rng::new(0x5111);
    let mut checked = 0;
    for _ in 0..200 {
        let big = gen_json_profiled(&mut rng, SizeProfile::Large);
        if !contains_number(&big) {
            continue;
        }
        checked += 1;
        let (_, small) = shrink_case(&Filter::Dot, &big, |_, i| contains_number(i), 5_000);
        assert!(
            contains_number(&small),
            "shrinking lost the failure: {small:?}"
        );
        assert!(
            json_nodes(&small) <= 2,
            "expected a near-minimal witness, got {} nodes from {} nodes",
            json_nodes(&small),
            json_nodes(&big)
        );
    }
    assert!(checked > 50, "generator produced too few numeric inputs");
}

/// Deep chains shrink down through the nesting.
#[test]
fn shrinks_deep_inputs() {
    let mut rng = Rng::new(0x5222);
    for _ in 0..100 {
        let deep = gen_json_profiled(&mut rng, SizeProfile::Deep);
        let (_, small) = shrink_case(&Filter::Dot, &deep, |_, i| json_nodes(i) >= 1, 5_000);
        assert!(json_nodes(&small) <= 1, "got {} nodes", json_nodes(&small));
    }
}

/// Program shrinking preserves the predicate and reduces the AST.
#[test]
fn shrinks_programs_preserving_predicate() {
    use tjq_testing::filtergen::gen_filter;
    let mut rng = Rng::new(0x5333);
    let mut checked = 0;
    for _ in 0..500 {
        let f = gen_filter(&mut rng, 5);
        if !contains_mod(&f) {
            continue;
        }
        checked += 1;
        let (small, _) = shrink_case(&f, &Json::Null, |f, _| contains_mod(f), 5_000);
        assert!(contains_mod(&small), "shrinking lost the % operator");
        // The minimal witness is `X % Y` with leaf operands
        if let Filter::BinOp(l, BinOp::Mod, r) = &small {
            assert!(
                matches!(l.as_ref(), Filter::Dot | Filter::Null)
                    && matches!(r.as_ref(), Filter::Dot | Filter::Null),
                "expected minimal operands, got {small:?}"
            );
        } else {
            panic!("expected a bare %, got {small:?}");
        }
    }
    assert!(checked > 20, "generator produced too few % programs");
}
