//! Random JSON value generation with small constant pools, so that random
//! shapes and random values collide often enough to exercise both the
//! satisfying and non-satisfying paths of every predicate.

use crate::rng::Rng;
use tjq_exec::Json;

/// Shared pools: `shapegen` draws singleton types from the same pools, which
/// is what makes `Number(Some(1.0))` meet `Json::Number(1.0)` in practice.
/// Kept small and collision-friendly (common integers and simple fractions)
/// so a random literal and a random input value coincide often; the weird
/// magnitudes live in `EXTREME_NUMBER_POOL`.
pub const NUMBER_POOL: [f64; 13] = [
    0.0, 1.0, -1.0, 2.0, -2.0, 0.5, -0.5, 0.25, 3.0, 4.0, 5.0, 10.0, 100.0,
];
/// Strings mix collision-friendly short tokens with Hypothesis-style "nasty"
/// values: keyword/number look-alikes, whitespace, quotes/backslash, and
/// multi-byte Unicode (to exercise codepoint-vs-byte length, `explode`, etc.).
pub const STRING_POOL: [&str; 18] = [
    "", "a", "b", "k", "one", "a b", " ", "0", "1", "null", "true", "false",
    "\"", "\\", "é", "λ", "😀", "日本",
];
/// Object keys overlap with `.key` accesses, so this stays modest to keep the
/// access hit-rate meaningful; a couple of extras plus a Unicode key.
pub const KEY_POOL: [&str; 7] = ["a", "b", "c", "k", "x", "y", "é"];

/// IEEE-double edge cases, following Hypothesis's "nasty floats": overflow-
/// adjacent magnitudes, `f64`/`f32` limits, subnormals, machine epsilons,
/// integer-precision boundary (2^53 ± 1), power-of-two int boundaries, awkward
/// fractions, and negative zero. All finite, so all valid JSON.
pub const EXTREME_NUMBER_POOL: [f64; 27] = [
    // Overflow-adjacent and type limits.
    1e308,
    -1e308,
    1.7976931348623157e308,  // f64::MAX
    -1.7976931348623157e308, // -f64::MAX
    3.402823466e38,          // f32::MAX
    1e15,
    1e17, // jq integer/exponent formatting boundary
    // Subnormals / smallest magnitudes.
    5e-324,                  // smallest positive subnormal
    2.2250738585072014e-308, // smallest positive normal
    1e-308,
    // Machine epsilons.
    2.220446049250313e-16,   // f64 epsilon
    1.1920928955078125e-7,   // f32 epsilon
    // Integer-precision boundary.
    9007199254740992.0,  // 2^53
    9007199254740993.0,  // 2^53 + 1 (not representable; rounds to 2^53)
    -9007199254740992.0, // -2^53
    // Power-of-two / integer-type boundaries.
    2147483647.0,  // 2^31 - 1
    2147483648.0,  // 2^31
    -2147483648.0, // -2^31
    4294967296.0,  // 2^32
    65536.0,       // 2^16
    // Awkward fractions near integers.
    0.1,
    0.3333333333333333, // 1/3
    1.1,
    1.5,
    0.999999,
    2.000001,
    // Signed zero.
    -0.0,
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
