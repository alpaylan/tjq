//! Properties of the inhabitant generator and the near-miss mutator.

use tjq_testing::denotes;
use tjq_testing::inhabit::{inhabit, mutate};
use tjq_testing::rng::Rng;
use tjq_testing::shapegen::gen_shape;

const CASES: usize = 20_000;

/// Every value `inhabit` returns must satisfy the shape it was asked for.
#[test]
fn inhabitants_satisfy_their_shape() {
    let mut rng = Rng::new(0x1AB1);
    for case in 0..CASES {
        let s = gen_shape(&mut rng, 3);
        if let Some(j) = inhabit(&s, &mut rng) {
            assert!(
                denotes(&s, &j),
                "case {case}: inhabit({s}) produced {j:?} which fails check"
            );
        }
    }
}

/// The generator must actually succeed often enough to be useful. Empty
/// types (e.g. contradictory intersections) legitimately return None, but
/// the bulk of random denotable shapes are inhabited.
#[test]
fn inhabitation_rate_is_useful() {
    let mut rng = Rng::new(0x2AB2);
    let mut inhabited = 0usize;
    for _ in 0..CASES {
        let s = gen_shape(&mut rng, 3);
        if inhabit(&s, &mut rng).is_some() {
            inhabited += 1;
        }
    }
    let rate = inhabited as f64 / CASES as f64;
    assert!(
        rate > 0.80,
        "inhabitation rate {rate:.3} dropped below the useful floor"
    );
}

/// Mutation must produce escapees (values that leave the shape) at a
/// useful rate; those are the near-miss non-satisfying inputs the
/// differential rig feeds to jq.
#[test]
fn mutation_produces_escapees() {
    let mut rng = Rng::new(0x3AB3);
    let mut attempts = 0usize;
    let mut escapees = 0usize;
    for _ in 0..CASES {
        let s = gen_shape(&mut rng, 3);
        // Blob-like shapes accept everything; no mutant can escape. Only
        // count shapes that constrain at all.
        if let Some(j) = inhabit(&s, &mut rng) {
            attempts += 1;
            for _ in 0..4 {
                let m = mutate(&j, &mut rng);
                if !denotes(&s, &m) {
                    escapees += 1;
                    break;
                }
            }
        }
    }
    let rate = escapees as f64 / attempts as f64;
    assert!(
        rate > 0.35,
        "escape rate {rate:.3} dropped below the useful floor \
         ({escapees}/{attempts})"
    );
}
