//! Random JSON value generation with small constant pools, so that random
//! shapes and random values collide often enough to exercise both the
//! satisfying and non-satisfying paths of every predicate.

use crate::rng::Rng;
use tjq_exec::Json;

/// Shared pools: `shapegen` draws singleton types from the same pools, which
/// is what makes `Number(Some(1.0))` meet `Json::Number(1.0)` in practice.
pub const NUMBER_POOL: [f64; 7] = [0.0, 1.0, -1.0, 2.0, 0.5, 3.0, 100.0];
pub const STRING_POOL: [&str; 5] = ["", "a", "b", "one", "k"];
pub const KEY_POOL: [&str; 4] = ["a", "b", "k", "x"];

/// IEEE-double edge cases: overflow-adjacent, subnormal, integer-precision
/// boundary (2^53 ± 1), and negative zero. All finite (valid JSON).
pub const EXTREME_NUMBER_POOL: [f64; 8] = [
    1e308,
    -1e308,
    5e-324,
    9007199254740992.0, // 2^53
    9007199254740993.0, // 2^53 + 1 (not representable; rounds to 2^53)
    -0.0,
    1e15,
    0.1,
];

/// How big generated values should be. `Small` is the collision-friendly
/// default the model layer relies on; the rest exercise jq's allocation,
/// recursion, and numeric edge paths. `Deep` stays under ~100 levels: both
/// jq's input parser and serde_json (used to read jq's output back) have
/// nesting limits near 128.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizeProfile {
    Small,
    Large,
    Deep,
    Degenerate,
}

/// Generate a value according to the profile.
pub fn gen_json_profiled(rng: &mut Rng, profile: SizeProfile) -> Json {
    match profile {
        SizeProfile::Small => gen_json(rng, 3),
        SizeProfile::Large => gen_wide(rng, 2),
        SizeProfile::Deep => {
            let levels = 16 + rng.below(80);
            gen_deep(rng, levels)
        }
        SizeProfile::Degenerate => gen_degenerate(rng),
    }
}

/// Wide containers: arrays/objects with 8-64 children, shallow nesting.
fn gen_wide(rng: &mut Rng, depth: usize) -> Json {
    if depth == 0 {
        return gen_scalar(rng);
    }
    if rng.chance(2, 3) {
        let len = 8 + rng.below(57);
        Json::Array((0..len).map(|_| gen_wide(rng, depth - 1)).collect())
    } else {
        let len = 8 + rng.below(25);
        Json::Object(
            (0..len)
                .map(|i| (format!("k{i}"), gen_wide(rng, depth - 1)))
                .collect(),
        )
    }
}

/// A single chain of nested containers with a scalar at the bottom.
fn gen_deep(rng: &mut Rng, levels: usize) -> Json {
    let mut value = gen_scalar(rng);
    for _ in 0..levels {
        value = if rng.chance(1, 2) {
            Json::Array(vec![value])
        } else {
            Json::Object(vec![(rng.pick(&KEY_POOL).to_string(), value)])
        };
    }
    value
}

/// Scalar extremes: boundary numbers, long strings, unicode- and
/// escape-heavy strings.
fn gen_degenerate(rng: &mut Rng) -> Json {
    match rng.below(4) {
        0 => Json::Number(*rng.pick(&EXTREME_NUMBER_POOL)),
        1 => Json::String("a".repeat(512 + rng.below(7_500))),
        2 => Json::String("λ🦀\u{200d}é\u{0301}".repeat(64 + rng.below(200))),
        _ => Json::String("\"\\\n\t\r\u{1}".repeat(32 + rng.below(100))),
    }
}

fn gen_scalar(rng: &mut Rng) -> Json {
    match rng.below(5) {
        0 => Json::Null,
        1 => Json::Boolean(rng.chance(1, 2)),
        2 => Json::Number(*rng.pick(&NUMBER_POOL)),
        3 => Json::Number(*rng.pick(&EXTREME_NUMBER_POOL)),
        _ => Json::String(rng.pick(&STRING_POOL).to_string()),
    }
}

pub fn gen_json(rng: &mut Rng, depth: usize) -> Json {
    let leaf_only = depth == 0;
    let choice = if leaf_only {
        rng.below(4)
    } else {
        rng.below(6)
    };
    match choice {
        0 => Json::Null,
        1 => Json::Boolean(rng.chance(1, 2)),
        2 => Json::Number(*rng.pick(&NUMBER_POOL)),
        3 => Json::String(rng.pick(&STRING_POOL).to_string()),
        4 => {
            let len = rng.below(4);
            Json::Array((0..len).map(|_| gen_json(rng, depth - 1)).collect())
        }
        _ => {
            let len = rng.below(4);
            let mut fields: Vec<(String, Json)> = vec![];
            for _ in 0..len {
                let key = rng.pick(&KEY_POOL).to_string();
                if !fields.iter().any(|(k, _)| k == &key) {
                    fields.push((key, gen_json(rng, depth - 1)));
                }
            }
            Json::Object(fields)
        }
    }
}
