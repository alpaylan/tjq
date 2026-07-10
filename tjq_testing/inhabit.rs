//! Inhabitant generation: the (partial) inverse of `Shape::check`.
//! `inhabit` produces a JSON value satisfying the shape, or `None` when it
//! cannot find one (empty type, or search exhausted). Every returned value
//! is post-checked, so `inhabit(s) == Some(j)` implies `denotes(s, j)`.

use crate::denotes;
use crate::jsongen::{
    gen_json, gen_json_profiled, SizeProfile, EXTREME_NUMBER_POOL, NUMBER_POOL, STRING_POOL,
};
use crate::rng::Rng;
use tjq_exec::Json;
use tjq_semantics::Shape;

const NEG_SEARCH_TRIES: usize = 32;
const INTERSECTION_TRIES: usize = 16;

pub fn inhabit(shape: &Shape, rng: &mut Rng) -> Option<Json> {
    inhabit_profiled(shape, rng, SizeProfile::Small)
}

/// Inhabit a shape, using the profile wherever the shape leaves room: the
/// unconstrained parts (Blob/TVar), element counts of unbounded arrays, and
/// the choice of witnesses for `number`/`string`.
pub fn inhabit_profiled(shape: &Shape, rng: &mut Rng, profile: SizeProfile) -> Option<Json> {
    let candidate = inhabit_raw(shape, rng, 3, profile)?;
    if denotes(shape, &candidate) {
        Some(candidate)
    } else {
        // The constructive path produced a wrong value (e.g. an intersection
        // component we sampled from was too weak); fall back to search.
        search(shape, rng)
    }
}

fn inhabit_raw(shape: &Shape, rng: &mut Rng, depth: usize, profile: SizeProfile) -> Option<Json> {
    match shape {
        // Unconstrained: any JSON value
        Shape::Blob | Shape::TVar(_) => Some(gen_json_profiled(rng, profile)),
        Shape::Null => Some(Json::Null),
        Shape::Bool(None) => Some(Json::Boolean(rng.chance(1, 2))),
        Shape::Bool(Some(b)) => Some(Json::Boolean(*b)),
        Shape::Number(None) => Some(Json::Number(match profile {
            SizeProfile::Degenerate => *rng.pick(&EXTREME_NUMBER_POOL),
            _ => *rng.pick(&NUMBER_POOL),
        })),
        Shape::Number(Some(n)) => Some(Json::Number(*n)),
        Shape::String(None) => Some(Json::String(match profile {
            SizeProfile::Degenerate => "a".repeat(512 + rng.below(4_000)),
            _ => rng.pick(&STRING_POOL).to_string(),
        })),
        Shape::String(Some(s)) => Some(Json::String(s.clone())),
        Shape::Array(elem, len) => {
            let n = match len {
                Some(n) => (*n).max(0) as usize,
                None => match profile {
                    SizeProfile::Large => 8 + rng.below(57),
                    _ => rng.below(3),
                },
            };
            let items: Option<Vec<Json>> = (0..n)
                .map(|_| inhabit_raw(elem, rng, depth.saturating_sub(1), profile))
                .collect();
            Some(Json::Array(items?))
        }
        Shape::Tuple(elems) => {
            let items: Option<Vec<Json>> = elems
                .iter()
                .map(|s| inhabit_raw(s, rng, depth.saturating_sub(1), profile))
                .collect();
            Some(Json::Array(items?))
        }
        Shape::Object(row) => {
            let mut items: Vec<(String, Json)> = vec![];
            for field in row.iter() {
                // An optional field is sometimes omitted; a required one is
                // always present.
                if field.optional && rng.chance(1, 2) {
                    continue;
                }
                let v = inhabit_raw(&field.value, rng, depth.saturating_sub(1), profile)?;
                items.push((field.key.clone(), v));
            }
            // An open row may carry extra keys not named in the type.
            if row.open && rng.chance(1, 4) {
                let extra = format!("extra{}", rng.below(1000));
                if row.get(&extra).is_none() {
                    items.push((extra, gen_json(rng, 1)));
                }
            }
            Some(Json::Object(items))
        }
        Shape::Union(s1, s2) => {
            let (first, second) = if rng.chance(1, 2) { (s1, s2) } else { (s2, s1) };
            inhabit_raw(first, rng, depth, profile)
                .or_else(|| inhabit_raw(second, rng, depth, profile))
        }
        Shape::Intersection(s1, s2) => {
            // Sample candidates from either side and keep those the whole
            // intersection accepts.
            for _ in 0..INTERSECTION_TRIES {
                let side = if rng.chance(1, 2) { s1 } else { s2 };
                if let Some(candidate) = inhabit_raw(side, rng, depth, profile) {
                    if denotes(shape, &candidate) {
                        return Some(candidate);
                    }
                }
            }
            None
        }
        Shape::Neg(inner) => {
            // Rejection-sample random values until one avoids the inner shape.
            for _ in 0..NEG_SEARCH_TRIES {
                let candidate = gen_json(rng, 2);
                if !denotes(inner, &candidate) {
                    return Some(candidate);
                }
            }
            None
        }
        // Empty / non-value types have no inhabitants
        Shape::Mismatch(_, _) | Shape::Arrow(_, _) => None,
    }
}

/// Last-resort search: random values filtered by the full predicate.
fn search(shape: &Shape, rng: &mut Rng) -> Option<Json> {
    for _ in 0..NEG_SEARCH_TRIES {
        let candidate = gen_json(rng, 2);
        if denotes(shape, &candidate) {
            return Some(candidate);
        }
    }
    None
}

/// Produce a structural near-miss: a value close to `j` (one edit away).
/// Callers filter the result against the target shape; a mutant that still
/// satisfies the shape is discarded by the caller, not here.
pub fn mutate(j: &Json, rng: &mut Rng) -> Json {
    match (j, rng.below(4)) {
        // Point mutations at this node
        (Json::Number(n), 0) => Json::Number(n + 1.0),
        (Json::Number(n), 1) => Json::Number(n + 0.5),
        (Json::Number(_), _) => Json::String("mutant".to_string()),
        (Json::String(s), 0) => Json::String(format!("{s}!")),
        (Json::String(_), _) => Json::Number(42.0),
        (Json::Boolean(b), 0) => Json::Boolean(!b),
        (Json::Boolean(_), _) => Json::Null,
        (Json::Null, _) => Json::Number(0.0),
        (Json::Array(arr), c) => {
            if arr.is_empty() {
                return Json::Number(7.0);
            }
            let mut arr = arr.clone();
            match c {
                // Drop an element
                0 => {
                    let i = rng.below(arr.len());
                    arr.remove(i);
                }
                // Mutate an element
                _ => {
                    let i = rng.below(arr.len());
                    arr[i] = mutate(&arr[i], rng);
                }
            }
            Json::Array(arr)
        }
        (Json::Object(fields), c) => {
            if fields.is_empty() {
                return Json::Array(vec![]);
            }
            let mut fields = fields.clone();
            match c {
                // Drop a field
                0 => {
                    let i = rng.below(fields.len());
                    fields.remove(i);
                }
                // Mutate a field value
                _ => {
                    let i = rng.below(fields.len());
                    let (k, v) = &fields[i];
                    fields[i] = (k.clone(), mutate(v, rng));
                }
            }
            Json::Object(fields)
        }
    }
}
