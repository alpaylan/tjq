//! Model tests for the shape algebra (docs/type-system-scope.md §11.1).
//!
//! `Shape::check` is the denotation: `denotes(s, j)` means the JSON value
//! `j` inhabits the set `s` describes. Every algebraic operation must agree
//! with the model:
//!   - set operations are boolean algebra on denotations
//!   - `canonicalize` must preserve denotations exactly
//!   - `subtype` must imply denotation inclusion
//!
//! Failures print the seed and the offending shape/value; rerun with the
//! seed to reproduce.

use tjq_testing::denotes;
use tjq_testing::jsongen::gen_json;
use tjq_testing::rng::Rng;
use tjq_testing::shapegen::gen_shape;

use tjq_semantics::{Shape, Subtyping};

fn cases() -> usize {
    std::env::var("TJQ_MODEL_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(20_000)
}

fn base_seed(default: u64) -> u64 {
    std::env::var("TJQ_MODEL_SEED")
        .ok()
        .and_then(|v| v.parse().ok())
        .map(|s: u64| s.wrapping_mul(0x9E37).wrapping_add(default))
        .unwrap_or(default)
}

const JSON_SAMPLES: usize = 8;

#[test]
fn from_json_denotes_itself() {
    let mut rng = Rng::new(base_seed(0xF00D));
    for case in 0..cases() {
        let j = gen_json(&mut rng, 3);
        let s = Shape::from_json(j.clone());
        assert!(
            denotes(&s, &j),
            "case {case}: from_json({j:?}) = {s:?} does not accept its own value"
        );
    }
}

#[test]
fn set_operations_are_boolean_algebra() {
    let mut rng = Rng::new(base_seed(0xBEEF));
    for case in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        let j = gen_json(&mut rng, 2);

        let da = denotes(&a, &j);
        let db = denotes(&b, &j);

        let union = Shape::Union(Box::new(a.clone()), Box::new(b.clone()));
        assert_eq!(
            denotes(&union, &j),
            da || db,
            "case {case}: union law failed for {a} | {b} on {j:?}"
        );

        let inter = Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()));
        assert_eq!(
            denotes(&inter, &j),
            da && db,
            "case {case}: intersection law failed for {a} & {b} on {j:?}"
        );

        let neg = Shape::Neg(Box::new(a.clone()));
        assert_eq!(
            denotes(&neg, &j),
            !da,
            "case {case}: negation law failed for !{a} on {j:?}"
        );
    }
}

#[test]
fn canonicalize_preserves_denotation() {
    let mut rng = Rng::new(base_seed(0xCAFE));
    for case in 0..cases() {
        let s = gen_shape(&mut rng, 3);
        let canon = s.canonicalize();
        for _ in 0..JSON_SAMPLES {
            let j = gen_json(&mut rng, 2);
            let before = denotes(&s, &j);
            let after = denotes(&canon, &j);
            assert_eq!(
                before, after,
                "case {case}: canonicalize changed denotation on {j:?}\n  \
                 original:  {s}\n  canonical: {canon}\n  \
                 before={before} after={after}"
            );
        }
    }
}

/// If canonicalizing an intersection claims emptiness (Mismatch), the two
/// components must genuinely share no inhabitants.
#[test]
fn intersection_mismatch_means_empty() {
    let mut rng = Rng::new(base_seed(0xD15C0));
    for case in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        let inter = Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()));
        if matches!(inter.canonicalize(), Shape::Mismatch(_, _)) {
            for _ in 0..JSON_SAMPLES {
                let j = gen_json(&mut rng, 2);
                assert!(
                    !(denotes(&a, &j) && denotes(&b, &j)),
                    "case {case}: {a} & {b} canonicalized to Mismatch (empty) \
                     but {j:?} inhabits both"
                );
            }
        }
    }
}

/// Explore which reading of the tri-state `subtype` matches the model.
/// Run manually: cargo test -p tjq_testing subtype_direction -- --ignored --nocapture
#[test]
#[ignore]
fn subtype_direction_report() {
    let mut rng = Rng::new(base_seed(0x5EED));
    // violations[result][direction]: direction 0 = "a ⊆ b", 1 = "b ⊆ a"
    let mut totals = [0usize; 3];
    let mut violations = [[0usize; 2]; 3];
    let label = |r: &Subtyping| match r {
        Subtyping::Subtype => 0,
        Subtyping::Supertype => 1,
        Subtyping::Incompatible => 2,
    };
    for _ in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        let r = label(&a.subtype(&b));
        totals[r] += 1;
        for _ in 0..JSON_SAMPLES {
            let j = gen_json(&mut rng, 2);
            let da = denotes(&a, &j);
            let db = denotes(&b, &j);
            if da && !db {
                violations[r][0] += 1; // a ⊄ b witnessed
            }
            if db && !da {
                violations[r][1] += 1; // b ⊄ a witnessed
            }
        }
    }
    println!("result       count   viol(a⊆b)   viol(b⊆a)");
    for (i, name) in ["Subtype", "Supertype", "Incompatible"].iter().enumerate() {
        println!(
            "{name:<12} {:>6}  {:>9}  {:>9}",
            totals[i], violations[i][0], violations[i][1]
        );
    }
}

/// `included_in` is standard semantic inclusion: a claim of `a ⊆ b` must
/// mean every inhabitant of `a` inhabits `b`.
#[test]
fn included_in_implies_inclusion() {
    let mut rng = Rng::new(base_seed(0x1DEA));
    for case in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        if a.included_in(&b) {
            for _ in 0..JSON_SAMPLES {
                let j = gen_json(&mut rng, 2);
                if denotes(&a, &j) {
                    assert!(
                        denotes(&b, &j),
                        "case {case}: included_in claimed {a} ⊆ {b}, \
                         but {j:?} inhabits only the left side"
                    );
                }
            }
        }
    }
}

/// `disjoint_with` claims must mean no common inhabitant.
#[test]
fn disjoint_with_implies_no_overlap() {
    let mut rng = Rng::new(base_seed(0xD105));
    for case in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        if a.disjoint_with(&b) {
            for _ in 0..JSON_SAMPLES {
                let j = gen_json(&mut rng, 2);
                assert!(
                    !(denotes(&a, &j) && denotes(&b, &j)),
                    "case {case}: disjoint_with claimed {a} ∩ {b} = ∅, \
                     but {j:?} inhabits both"
                );
            }
        }
    }
}

/// The legacy tri-state `subtype` uses an *inverted* convention (see its
/// doc comment): `Subtype` claims `a ⊇ b`, `Supertype` claims `a ⊂ b`.
/// `Incompatible` makes no claim. The solver depends on these readings.
#[test]
fn legacy_subtype_claims_hold() {
    let mut rng = Rng::new(base_seed(0x5EED));
    for case in 0..cases() {
        let a = gen_shape(&mut rng, 2);
        let b = gen_shape(&mut rng, 2);
        match a.subtype(&b) {
            Subtyping::Subtype => {
                // claim: a ⊇ b
                for _ in 0..JSON_SAMPLES {
                    let j = gen_json(&mut rng, 2);
                    if denotes(&b, &j) {
                        assert!(
                            denotes(&a, &j),
                            "case {case}: subtype({a}, {b}) = Subtype claims \
                             the left covers the right, but {j:?} inhabits \
                             only the right"
                        );
                    }
                }
            }
            Subtyping::Supertype => {
                // claim: a ⊂ b
                for _ in 0..JSON_SAMPLES {
                    let j = gen_json(&mut rng, 2);
                    if denotes(&a, &j) {
                        assert!(
                            denotes(&b, &j),
                            "case {case}: subtype({a}, {b}) = Supertype claims \
                             the right covers the left, but {j:?} inhabits \
                             only the left"
                        );
                    }
                }
            }
            Subtyping::Incompatible => {}
        }
    }
}
