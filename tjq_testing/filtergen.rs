//! Random jq program generation over the `Filter` AST, plus a
//! precedence-safe pretty-printer. `Filter`'s `Display` cannot be used as a
//! source printer: it emits string literals unquoted and never
//! parenthesizes, so printed programs re-parse to different trees.
//!
//! The palette targets the subset the experimental inference handles;
//! constructs it panics on are still findings (the runner catches and
//! counts them), but weights favor productive programs. `error`/`empty`
//! are excluded until the failure effect lands in the arrow types
//! (docs/type-system-scope.md §2).

use crate::jsongen::{EXTREME_NUMBER_POOL, KEY_POOL, NUMBER_POOL, STRING_POOL};
use crate::rng::Rng;
use tjq_exec::{BinOp, Filter, UnOp};

pub fn gen_filter(rng: &mut Rng, depth: usize) -> Filter {
    if depth == 0 {
        return gen_leaf(rng);
    }
    match rng.below(29) {
        // Leaves stay likely at every depth so programs end
        0..=5 => gen_leaf(rng),
        6..=9 => Filter::Pipe(
            Box::new(gen_filter(rng, depth - 1)),
            Box::new(gen_filter(rng, depth - 1)),
        ),
        10..=12 => {
            let op = *rng.pick(&[BinOp::Add, BinOp::Sub, BinOp::Mul, BinOp::Div, BinOp::Mod]);
            Filter::BinOp(
                Box::new(gen_filter(rng, depth - 1)),
                op,
                Box::new(gen_filter(rng, depth - 1)),
            )
        }
        13..=14 => {
            let op = *rng.pick(&[
                BinOp::Eq,
                BinOp::Ne,
                BinOp::Gt,
                BinOp::Lt,
                BinOp::Ge,
                BinOp::Le,
            ]);
            Filter::BinOp(
                Box::new(gen_filter(rng, depth - 1)),
                op,
                Box::new(gen_filter(rng, depth - 1)),
            )
        }
        15..=16 => Filter::IfThenElse(
            Box::new(gen_filter(rng, depth - 1)),
            Box::new(gen_filter(rng, depth - 1)),
            Box::new(gen_filter(rng, depth - 1)),
        ),
        17 => Filter::Comma(
            Box::new(gen_filter(rng, depth - 1)),
            Box::new(gen_filter(rng, depth - 1)),
        ),
        18 => {
            let len = rng.below(3);
            Filter::Array((0..len).map(|_| gen_filter(rng, depth - 1)).collect())
        }
        19 => Filter::UnOp(UnOp::Neg, Box::new(gen_filter(rng, depth - 1))),
        // Input-consuming pipelines: start from a field access or iterator
        // so the program constrains its input type
        20..=21 => Filter::Pipe(
            Box::new(gen_access(rng)),
            Box::new(gen_filter(rng, depth - 1)),
        ),
        22 => {
            let op = *rng.pick(&[BinOp::And, BinOp::Or]);
            Filter::BinOp(
                Box::new(gen_filter(rng, depth - 1)),
                op,
                Box::new(gen_filter(rng, depth - 1)),
            )
        }
        23 => {
            // Higher-order builtins taking a filter argument (defs.jq),
            // exercising jq's Call machinery.
            let name = *rng.pick(&["map", "select"]);
            Filter::Call(
                name.to_string(),
                Some(vec![gen_filter(rng, depth - 1)]),
            )
        }
        24 => {
            // `f?` — postfix error suppression. Lights up the VM's
            // FORK_OPT / backtrack-on-error opcodes.
            Filter::TryCatch(Box::new(gen_filter(rng, depth - 1)), None)
        }
        25 => {
            // `try f catch g`. The handler must NOT observe the error value:
            // tjq's error *messages* differ from jq's, so a handler that reads
            // its input (`.`, `length`, …) would diverge on message text
            // without being a real bug. A constant leaf keeps it comparable
            // while still exercising the TRY_BEGIN/END + catch path.
            Filter::TryCatch(
                Box::new(gen_filter(rng, depth - 1)),
                Some(Box::new(gen_const_leaf(rng))),
            )
        }
        26 => {
            // `VALUES as $v | BODY` — variable binding, exercising the VM's
            // STOREV/LOADV opcodes. The body references `$v` so the binding is
            // meaningful; `$v` only ever appears inside its own binding's body,
            // so there are no free (compile-rejected) variables.
            let var = (*rng.pick(&["x", "y", "v"])).to_string();
            let values = gen_filter(rng, depth - 1);
            let vref = || Filter::Variable(var.clone());
            let body = match rng.below(3) {
                0 => vref(),
                1 => Filter::BinOp(
                    Box::new(vref()),
                    *rng.pick(&[BinOp::Add, BinOp::Sub, BinOp::Eq, BinOp::Gt]),
                    Box::new(gen_filter(rng, depth - 1)),
                ),
                _ => Filter::Comma(Box::new(vref()), Box::new(gen_filter(rng, depth - 1))),
            };
            Filter::Pipe(
                Box::new(Filter::BindingExpression(Box::new(values), Box::new(vref()))),
                Box::new(body),
            )
        }
        27 => {
            // `reduce`/`foreach SOURCE as $v (INIT; UPDATE[; EXTRACT])` — the
            // fold constructs, exercising the VM's FORK/STOREV/backtrack path.
            // UPDATE references the accumulator (`.`) and the bound `$v`.
            let var = (*rng.pick(&["x", "y", "v"])).to_string();
            let vref = || Filter::Variable(var.clone());
            let source = match rng.below(2) {
                0 => Filter::ArrayIterator,
                _ => Filter::Comma(
                    Box::new(gen_filter(rng, depth - 1)),
                    Box::new(gen_filter(rng, depth - 1)),
                ),
            };
            let init = gen_const_leaf(rng);
            let update = match rng.below(3) {
                0 => Filter::BinOp(
                    Box::new(Filter::Dot),
                    *rng.pick(&[BinOp::Add, BinOp::Sub, BinOp::Mul]),
                    Box::new(vref()),
                ),
                1 => vref(),
                _ => Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Add,
                    Box::new(Filter::Array(vec![vref()])),
                ),
            };
            if rng.chance(1, 2) {
                Filter::ReduceExpression(
                    var,
                    Box::new(source),
                    Box::new(init),
                    Box::new(update),
                )
            } else {
                let extract = if rng.chance(1, 2) {
                    Some(Box::new(gen_filter(rng, depth - 1)))
                } else {
                    None
                };
                Filter::ForeachExpression(
                    var,
                    Box::new(source),
                    Box::new(init),
                    Box::new(update),
                    extract,
                )
            }
        }
        _ => {
            // Object construction with 1-2 literal keys
            let len = 1 + rng.below(2);
            let mut fields: Vec<(Filter, Filter)> = vec![];
            for _ in 0..len {
                let key = rng.pick(&KEY_POOL).to_string();
                if !fields
                    .iter()
                    .any(|(k, _)| matches!(k, Filter::String(s) if s == &key))
                {
                    fields.push((Filter::String(key), gen_filter(rng, depth - 1)));
                }
            }
            Filter::Object(fields)
        }
    }
}

/// An input-consuming access: `.a`, `.a | .b`, or `.[]`
fn gen_access(rng: &mut Rng) -> Filter {
    fn key(rng: &mut Rng) -> Filter {
        Filter::ObjIndex(Box::new(Filter::String(rng.pick(&KEY_POOL).to_string())))
    }
    match rng.below(4) {
        0 | 1 => key(rng),
        2 => {
            let first = key(rng);
            let second = key(rng);
            Filter::Pipe(Box::new(first), Box::new(second))
        }
        _ => Filter::ArrayIterator,
    }
}

/// A constant leaf that ignores its input entirely (a literal). Used for
/// `catch` handlers, whose input is the error value — which we must not
/// observe (see the `try f catch g` generator arm).
fn gen_const_leaf(rng: &mut Rng) -> Filter {
    match rng.below(5) {
        0 => Filter::Null,
        1 => Filter::Boolean(rng.chance(1, 2)),
        2 | 3 => Filter::Number(*rng.pick(&NUMBER_POOL)),
        _ => Filter::String(rng.pick(&STRING_POOL).to_string()),
    }
}

fn gen_leaf(rng: &mut Rng) -> Filter {
    match rng.below(15) {
        0 | 1 => Filter::Dot,
        2 | 3 => Filter::ObjIndex(Box::new(Filter::String(rng.pick(&KEY_POOL).to_string()))),
        4 => Filter::Null,
        5 => Filter::Boolean(rng.chance(1, 2)),
        6 => Filter::Number(*rng.pick(&NUMBER_POOL)),
        // Occasionally emit an IEEE edge-case literal so arithmetic and
        // comparison paths meet overflow/subnormal/precision-boundary values.
        7 => Filter::Number(if rng.chance(1, 3) {
            *rng.pick(&EXTREME_NUMBER_POOL)
        } else {
            *rng.pick(&NUMBER_POOL)
        }),
        8 => Filter::String(rng.pick(&STRING_POOL).to_string()),
        9 => Filter::ArrayIterator,
        10 => Filter::ArrayIndex(Box::new(Filter::Number(rng.below(3) as f64))),
        11 => Filter::Call("length".to_string(), None),
        12 => {
            let name = *rng.pick(&["type", "not", "tostring"]);
            Filter::Call(name.to_string(), None)
        }
        13 => {
            let name = *rng.pick(&["keys", "floor", "tonumber"]);
            Filter::Call(name.to_string(), None)
        }
        _ => {
            // Array-consuming builtins.
            let name = *rng.pick(&["add", "sort", "reverse", "min", "max", "flatten"]);
            Filter::Call(name.to_string(), None)
        }
    }
}

/// Print a `Filter` as jq source. Conservatively parenthesizes every
/// non-atomic subterm; jq accepts redundant parentheses everywhere.
pub fn to_jq_source(f: &Filter) -> String {
    match f {
        Filter::Dot => ".".to_string(),
        // `VALUES as $pat | BODY`: the binding must not be parenthesized
        // apart from its body (`(f as $x) | $x` is a jq compile error — the
        // binding's scope ends at the paren), so print it as one unit. When
        // this whole expression is an operand, `atom` wraps it as
        // `(… as $x | …)`, which jq accepts.
        Filter::Pipe(f1, f2) if matches!(f1.as_ref(), Filter::BindingExpression(_, _)) => {
            let Filter::BindingExpression(values, pat) = f1.as_ref() else {
                unreachable!()
            };
            format!("{} as {} | {}", atom(values), to_jq_source(pat), atom(f2))
        }
        Filter::Pipe(f1, f2) => format!("{} | {}", atom(f1), atom(f2)),
        Filter::Variable(name) => format!("${}", name),
        Filter::BindingExpression(values, pat) => {
            format!("{} as {}", atom(values), to_jq_source(pat))
        }
        Filter::ReduceExpression(var, gen, init, upd) => format!(
            "reduce {} as ${} ({}; {})",
            atom(gen),
            var,
            to_jq_source(init),
            to_jq_source(upd)
        ),
        Filter::ForeachExpression(var, gen, init, upd, extract) => match extract {
            Some(ex) => format!(
                "foreach {} as ${} ({}; {}; {})",
                atom(gen),
                var,
                to_jq_source(init),
                to_jq_source(upd),
                to_jq_source(ex)
            ),
            None => format!(
                "foreach {} as ${} ({}; {})",
                atom(gen),
                var,
                to_jq_source(init),
                to_jq_source(upd)
            ),
        },
        Filter::Comma(f1, f2) => format!("{}, {}", atom(f1), atom(f2)),
        Filter::ObjIndex(inner) => match inner.as_ref() {
            // `.foo` shorthand is only valid for identifier keys; other keys
            // (Unicode, whitespace, empty) need the bracket form `.["…"]`.
            Filter::String(s) if is_jq_ident(s) => format!(".{}", s),
            Filter::String(s) => format!(".[{}]", crate::escape_json_string(s)),
            other => format!(".[{}]", atom(other)),
        },
        Filter::ArrayIndex(inner) => format!(".[{}]", to_jq_source(inner)),
        Filter::ArrayIterator => ".[]".to_string(),
        Filter::Null => "null".to_string(),
        Filter::Boolean(b) => b.to_string(),
        // Negative literals are not atomic (see `atomic`); `atom` adds the
        // parentheses when they appear as operands.
        Filter::Number(n) => n.to_string(),
        // Rust's `escape_default` emits `\u{XXXX}` for non-ASCII, which jq
        // cannot parse; `escape_json_string` emits JSON-valid escapes.
        Filter::String(s) => crate::escape_json_string(s),
        Filter::Array(items) => {
            // Inside brackets, `,` and `|` bind across elements
            // (`[a, b | c]` is `[(a, b) | c]`), so elements are atomized.
            let inner: Vec<String> = items.iter().map(|i| atom(i)).collect();
            format!("[{}]", inner.join(", "))
        }
        Filter::Object(fields) => {
            let inner: Vec<String> = fields
                .iter()
                .map(|(k, v)| {
                    let key = match k {
                        // Bare identifier keys print unquoted; all others must
                        // be quoted (jq accepts `{"é": …}` but not `{é: …}`).
                        Filter::String(s) if is_jq_ident(s) => s.clone(),
                        Filter::String(s) => crate::escape_json_string(s),
                        other => format!("({})", to_jq_source(other)),
                    };
                    format!("{}: {}", key, atom(v))
                })
                .collect();
            format!("{{{}}}", inner.join(", "))
        }
        Filter::UnOp(op, inner) => format!("{}{}", op, atom(inner)),
        Filter::BinOp(l, op, r) => format!("{} {} {}", atom(l), op, atom(r)),
        Filter::IfThenElse(c, t, e) => format!(
            "if {} then {} else {} end",
            to_jq_source(c),
            to_jq_source(t),
            to_jq_source(e)
        ),
        // `f?` postfix; wrap the body so precedence is unambiguous.
        Filter::TryCatch(body, None) => format!("{}?", atom(body)),
        Filter::TryCatch(body, Some(handler)) => {
            format!("try {} catch {}", atom(body), atom(handler))
        }
        Filter::Call(name, None) => name.clone(),
        Filter::Call(name, Some(args)) => {
            let inner: Vec<String> = args.iter().map(to_jq_source).collect();
            format!("{}({})", name, inner.join("; "))
        }
        Filter::Empty => "empty".to_string(),
        Filter::Error => "error".to_string(),
        // Not generated; fall back to Display for completeness
        other => other.to_string(),
    }
}

/// Whether `s` is a bare jq identifier: `[A-Za-z_][A-Za-z0-9_]*`. Only such
/// keys may use the `.foo` / `{foo: …}` shorthands; everything else is quoted.
fn is_jq_ident(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn atomic(f: &Filter) -> bool {
    matches!(
        f,
        Filter::Dot
            | Filter::ObjIndex(_)
            | Filter::ArrayIndex(_)
            | Filter::ArrayIterator
            | Filter::Null
            | Filter::Boolean(_)
            | Filter::String(_)
            | Filter::Array(_)
            | Filter::Object(_)
            | Filter::Call(_, None)
            | Filter::Empty
            | Filter::Error
            | Filter::Variable(_)
    ) || matches!(f, Filter::Number(n) if n.is_sign_positive())
}

fn atom(f: &Filter) -> String {
    if atomic(f) {
        to_jq_source(f)
    } else {
        format!("({})", to_jq_source(f))
    }
}
