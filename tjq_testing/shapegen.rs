//! Random generation of *denotable* shapes: the subset of `Shape` that
//! describes sets of JSON values. `TVar` (solver-internal), `Arrow`
//! (function types), and `Mismatch` (error reports / empty) are excluded —
//! the algebra laws quantify over value types.

use crate::jsongen::{KEY_POOL, NUMBER_POOL, STRING_POOL};
use crate::rng::Rng;
use tjq_semantics::{Field, Row, Shape};

pub fn gen_shape(rng: &mut Rng, depth: usize) -> Shape {
    let choice = if depth == 0 {
        rng.below(8)
    } else {
        rng.below(13)
    };
    match choice {
        0 => Shape::Blob,
        1 => Shape::Null,
        2 => Shape::Bool(None),
        3 => Shape::Bool(Some(rng.chance(1, 2))),
        4 => Shape::Number(None),
        5 => Shape::Number(Some(*rng.pick(&NUMBER_POOL))),
        6 => Shape::String(None),
        7 => Shape::String(Some(rng.pick(&STRING_POOL).to_string())),
        8 => Shape::Array(Box::new(gen_shape(rng, depth - 1)), None),
        9 => {
            let len = rng.below(3);
            Shape::Tuple((0..len).map(|_| gen_shape(rng, depth - 1)).collect())
        }
        10 => {
            // Generate rows across all three new dimensions: field value
            // type, per-field optionality, and row openness.
            let len = rng.below(3);
            let mut fields: Vec<Field> = vec![];
            for _ in 0..len {
                let key = rng.pick(&KEY_POOL).to_string();
                if !fields.iter().any(|f| f.key == key) {
                    fields.push(Field {
                        key,
                        value: gen_shape(rng, depth - 1),
                        optional: rng.chance(1, 3),
                    });
                }
            }
            Shape::Object(Row {
                fields,
                open: rng.chance(1, 2),
            })
        }
        11 => Shape::Union(
            Box::new(gen_shape(rng, depth - 1)),
            Box::new(gen_shape(rng, depth - 1)),
        ),
        _ => {
            if rng.chance(1, 3) {
                Shape::Neg(Box::new(gen_shape(rng, depth - 1)))
            } else {
                Shape::Intersection(
                    Box::new(gen_shape(rng, depth - 1)),
                    Box::new(gen_shape(rng, depth - 1)),
                )
            }
        }
    }
}
