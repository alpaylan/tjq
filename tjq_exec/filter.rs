use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    vec,
};

use itertools::Itertools;

use crate::json::Json;
use crate::{error::JQError, filters};

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    Dot,                                                             // .
    Pipe(Box<Filter>, Box<Filter>),                                  // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),                                 // <f_1>, <f_2>
    ObjIndex(Box<Filter>),                                           // .<s>
    ArrayIndex(Box<Filter>),                                         // .[<n>]
    ArrayIterator,                                                   // .[]
    Null,                                                            // null
    Boolean(bool),                                                   // true | false
    Number(f64),                                                     // 1, 2..
    String(String),                                                  // "abc"
    Array(Vec<Filter>),                                              // [...]
    Object(Vec<(Filter, Filter)>),                                   // {...}
    UnOp(UnOp, Box<Filter>),                                         // <op> <f>
    BinOp(Box<Filter>, BinOp, Box<Filter>),                          // <f_1> <op> <f_2>
    Empty,                                                           // Empty
    Error,                                                           // Error
    Call(String, Option<Vec<Filter>>),                               // <s>(<f_1>, <f_2>...)
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>), // if <f_1> then <f_2> else <f_3>
    Bound(Vec<String>, Box<Filter>),                   // \<s_1>, <s_2>... <f>
    FunctionExpression(HashMap<String, Filter>, Box<Filter>), // local_defs, <f>
    BindingExpression(Box<Filter>, Box<Filter>),       //
    Variable(String),                                  // $var
    ReduceExpression(String, Box<Filter>, Box<Filter>, Box<Filter>), // reduce <f> as $<s> (<init>, <update>)
    // foreach <gen> as $<s> (<init>; <update>[; <extract>])
    ForeachExpression(
        String,
        Box<Filter>,
        Box<Filter>,
        Box<Filter>,
        Option<Box<Filter>>,
    ),
    SliceExpression(Option<Box<Filter>>, Option<Box<Filter>>), // .[start:end], .[start:], .[:end]
    TryCatch(Box<Filter>, Option<Box<Filter>>),                // try <f> [catch <g>]; `f?` is try <f>
    Alternative(Box<Filter>, Box<Filter>),                     // <f> // <g>
    Hole, // Placeholder for a missing value in the AST
}

pub fn builtin_filters() -> HashMap<String, Filter> {
    filters(include_str!("../tjq/defs.jq"))
}

/// Ceiling on a single data-driven allocation in the interpreter (string
/// repetition, and any future count-sized builder). 256 MiB is far beyond
/// anything jq produces within the differential harness's timeout, and far
/// below the astronomical sizes an extreme numeric count would demand.
pub const MAX_ALLOC_BYTES: usize = 256 * 1024 * 1024;

/// Ceiling on the number of values a single (sub)expression may materialize.
/// The interpreter is eager where jq is lazy, so a stream-multiplying program
/// (nested pipes, comma, object/array cartesian products) can blow up to sizes
/// jq would stream through lazily. Exceeding this yields
/// `AllocationTooLarge`, which the differential harness skips — so a resource
/// blowup becomes a non-finding instead of an unbounded hang.
pub const MAX_STREAM_LEN: usize = 4_000_000;

thread_local! {
    /// Set whenever the interpreter refuses a data-driven allocation
    /// (`AllocationTooLarge`). The differential harness resets this before each
    /// run and checks it after, so it can recognize a resource-limit artifact
    /// even when the program's own `?`/`catch` swallowed the guard error into a
    /// normal (empty or altered) output. Such a run is not a semantic
    /// divergence from jq, which attempts the (astronomical) allocation instead.
    static ALLOC_GUARD_TRIPPED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Clear the "allocation guard tripped" flag (call before an evaluation).
pub fn reset_alloc_guard() {
    ALLOC_GUARD_TRIPPED.with(|c| c.set(false));
}

/// Whether the interpreter's allocation guard fired since the last reset —
/// even if a `?`/`catch` intercepted the resulting error.
pub fn alloc_guard_tripped() -> bool {
    ALLOC_GUARD_TRIPPED.with(|c| c.get())
}

/// Construct an `AllocationTooLarge` error and record that the guard tripped.
fn alloc_too_large() -> JQError {
    ALLOC_GUARD_TRIPPED.with(|c| c.set(true));
    JQError::AllocationTooLarge
}

/// Apply a 1-argument native string/collection builtin to `input` with the
/// already-evaluated argument value `arg`. These are jq natives (no jq-level
/// definition). Error conditions match jq (only *whether* it errors matters
/// for the differential harness, which masks error values via `catch`).
fn apply_one_arg_string(name: &str, input: &Json, arg: &Json) -> Result<Json, JQError> {
    match name {
        // `has`: null → always false; object → key present (arg must be a
        // string); array → index truncated toward zero then range-checked
        // `0 ≤ idx < length` (jq: `has(1.5)`/`has(-0.5)` are true, `has(-1)`
        // false); every other input / arg kind errors.
        "has" => match input {
            Json::Null => Ok(Json::Boolean(false)),
            Json::Object(obj) => match arg {
                Json::String(k) => Ok(Json::Boolean(obj.iter().any(|(kk, _)| kk == k))),
                _ => Err(JQError::ObjIndexForNonObject(input.clone())),
            },
            Json::Array(arr) => match arg {
                Json::Number(n) => {
                    let idx = n.trunc();
                    Ok(Json::Boolean(idx >= 0.0 && idx < arr.len() as f64))
                }
                _ => Err(JQError::ObjIndexForNonObject(input.clone())),
            },
            _ => Err(JQError::ObjIndexForNonObject(input.clone())),
        },
        // `startswith`/`endswith`: both input and arg must be strings.
        "startswith" | "endswith" => match (input, arg) {
            (Json::String(s), Json::String(p)) => Ok(Json::Boolean(if name == "startswith" {
                s.starts_with(p.as_str())
            } else {
                s.ends_with(p.as_str())
            })),
            _ => Err(JQError::UnOpTypeError(input.clone(), UnOp::Neg)),
        },
        // `ltrimstr`/`rtrimstr`: strip the affix when input and arg are strings
        // and it matches; otherwise pass the input through unchanged (jq).
        "ltrimstr" => match (input, arg) {
            (Json::String(s), Json::String(p)) if s.starts_with(p.as_str()) => {
                Ok(Json::String(s[p.len()..].to_string()))
            }
            _ => Ok(input.clone()),
        },
        "rtrimstr" => match (input, arg) {
            (Json::String(s), Json::String(p)) if s.ends_with(p.as_str()) => {
                Ok(Json::String(s[..s.len() - p.len()].to_string()))
            }
            _ => Ok(input.clone()),
        },
        _ => unreachable!("apply_one_arg_string called with non-native {name}"),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Eq,  // ==
    Ne,  // !=
    Gt,  // >
    Ge,  // >=
    Lt,  // <
    Le,  // <=
    And, // and
    Or,  // or
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg, // -
}

impl Display for Filter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Filter::Dot => write!(f, "."),
            Filter::Pipe(f1, f2) => write!(f, "{} | {}", f1, f2),
            Filter::Alternative(f1, f2) => write!(f, "{} // {}", f1, f2),
            Filter::Comma(f1, f2) => write!(f, "{}, {}", f1, f2),
            Filter::ObjIndex(s) => write!(f, ".{}", s),
            Filter::ArrayIndex(i) => write!(f, ".[{}]", i),
            Filter::ArrayIterator => write!(f, ".[]"),
            Filter::Null => write!(f, "null"),
            Filter::Boolean(b) => write!(f, "{b}"),
            Filter::Number(n) => write!(f, "{n}"),
            Filter::String(s) => write!(f, "{s}"),
            Filter::Array(arr) => {
                write!(f, "[")?;
                for (i, j) in arr.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", j)?;
                }
                write!(f, "]")
            }
            Filter::Object(obj) => {
                write!(f, "{{")?;
                for (i, (key, value)) in obj.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            Filter::UnOp(un_op, filter) => {
                write!(f, "{} {}", un_op, filter)
            }
            Filter::BinOp(filter, bin_op, filter1) => {
                write!(f, "{} {} {}", filter, bin_op, filter1)
            }
            Filter::Empty => write!(f, "empty"),
            Filter::Error => write!(f, "error"),
            Filter::Call(name, filters) => {
                write!(f, "{}", name)?;
                if let Some(filters) = filters {
                    write!(f, "(")?;
                    for (i, filter) in filters.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", filter)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Filter::IfThenElse(filter, filter1, filter2) => {
                write!(f, "if {} then {} else {} end", filter, filter1, filter2)
            }
            Filter::Bound(_, filter) => {
                write!(f, " {}", filter)
            }
            Filter::FunctionExpression(local_defs, expr) => {
                for (name, local_filter) in local_defs {
                    write!(f, "def {name}: {local_filter}; ")?;
                }
                write!(f, "{})", expr)
            }
            Filter::BindingExpression(filter, pattern) => {
                write!(f, "{} as {}", filter, pattern)
            }
            Filter::Variable(s) => write!(f, "${s}"),
            Filter::ReduceExpression(var, gen, init, upd) => {
                write!(f, "reduce {} as ${} ({}; {})", gen, var, init, upd)
            }
            Filter::ForeachExpression(var, gen, init, upd, extract) => match extract {
                Some(ex) => write!(
                    f,
                    "foreach {} as ${} ({}; {}; {})",
                    gen, var, init, upd, ex
                ),
                None => write!(f, "foreach {} as ${} ({}; {})", gen, var, init, upd),
            },
            Filter::SliceExpression(start, end) => {
                write!(f, ".[")?;
                if let Some(s) = start {
                    write!(f, "{}", s)?;
                }
                write!(f, ":")?;
                if let Some(e) = end {
                    write!(f, "{}", e)?;
                }
                write!(f, "]")
            }
            Filter::TryCatch(body, None) => write!(f, "try {}", body),
            Filter::TryCatch(body, Some(handler)) => {
                write!(f, "try {} catch {}", body, handler)
            }
            Filter::Hole => write!(f, "(_)"),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
        }
    }
}

fn destructure_pattern(val: &Json, pat: &Filter, variable_ctx: &mut HashMap<String, Filter>) {
    match pat {
        // `$x` binds the whole value (any JSON type, held as a const filter).
        Filter::Variable(name) => {
            variable_ctx.insert(name.clone(), Filter::from_json_const(val));
        }
        // `[$a, $b, …]` binds each position; missing elements bind null.
        Filter::Array(pats) => {
            for (i, p) in pats.iter().enumerate() {
                let elem = match val {
                    Json::Array(arr) => arr.get(i).cloned().unwrap_or(Json::Null),
                    _ => Json::Null,
                };
                destructure_pattern(&elem, p, variable_ctx);
            }
        }
        // `{a: $x, …}` binds each field's value pattern; missing keys bind null.
        Filter::Object(pairs) => {
            for (key_pat, value_pat) in pairs {
                let field = match (key_pat, val) {
                    (Filter::String(k), Json::Object(obj)) => obj
                        .iter()
                        .find(|(kk, _)| kk == k)
                        .map(|(_, v)| v.clone())
                        .unwrap_or(Json::Null),
                    _ => Json::Null,
                };
                destructure_pattern(&field, value_pat, variable_ctx);
            }
        }
        _ => {}
    }
}

/// Collect the variable names a binding pattern introduces, so a caller can
/// save and restore the enclosing scope around the binding.
fn collect_pattern_vars(pat: &Filter, out: &mut Vec<String>) {
    match pat {
        Filter::Variable(name) => out.push(name.clone()),
        Filter::Array(pats) => pats.iter().for_each(|p| collect_pattern_vars(p, out)),
        Filter::Object(pairs) => pairs
            .iter()
            .for_each(|(_, v)| collect_pattern_vars(v, out)),
        _ => {}
    }
}

/// Check if a pattern binds a variable with the given name
fn pattern_binds_variable(pat: &Filter, var: &str) -> bool {
    match pat {
        Filter::Variable(name) => name == var,
        Filter::Array(pats) => pats.iter().any(|p| pattern_binds_variable(p, var)),
        Filter::Object(pairs) => pairs
            .iter()
            .any(|(_, value_pat)| pattern_binds_variable(value_pat, var)),
        _ => false,
    }
}

/// Constructor functions for easily constructing filters without boilerplate
impl Filter {
    pub fn pipe(f1: Filter, f2: Filter) -> Filter {
        Filter::Pipe(Box::new(f1), Box::new(f2))
    }

    pub fn comma(f1: Filter, f2: Filter) -> Filter {
        Filter::Comma(Box::new(f1), Box::new(f2))
    }

    pub fn if_then_else(cond: Filter, then: Filter, else_: Filter) -> Filter {
        Filter::IfThenElse(Box::new(cond), Box::new(then), Box::new(else_))
    }

    pub fn and(f1: Filter, f2: Filter) -> Filter {
        Filter::BinOp(Box::new(f1), BinOp::And, Box::new(f2))
    }

    pub fn or(f1: Filter, f2: Filter) -> Filter {
        Filter::BinOp(Box::new(f1), BinOp::Or, Box::new(f2))
    }

    pub fn eq(f1: Filter, f2: Filter) -> Filter {
        Filter::BinOp(Box::new(f1), BinOp::Eq, Box::new(f2))
    }
}

/// jq clamps infinite arithmetic results to the largest finite double
/// (`1e308 * 10` prints `1.7976931348623157e+308`, not an error).
pub(crate) fn clamp_number(n: f64) -> f64 {
    if n.is_infinite() {
        if n > 0.0 {
            f64::MAX
        } else {
            -f64::MAX
        }
    } else {
        n
    }
}

/// jq's `+` on two values: null is the identity, numbers add, strings and
/// arrays concatenate, objects merge (right-biased). Shared by `BinOp::Add`
/// Build the stream of objects for `{k1:v1, …}` with jq's exact semantics: the
/// first field is the outer loop, later fields nested inside, so completed
/// objects are appended to `out` in order and an error (a raised key/value, or
/// a non-string key) surfaces at its position — keeping the objects produced
/// before it, and never hidden by a later empty field. Returns `true` if an
/// error was appended (the caller stops), mirroring jq raising there.
fn build_object(
    fields: &[(Filter, Filter)],
    json: &Json,
    partial: Vec<(String, Json)>,
    globals: &HashMap<String, Filter>,
    var_ctx: &mut HashMap<String, Filter>,
    out: &mut Vec<Result<Json, JQError>>,
) -> bool {
    let Some(((kf, vf), rest)) = fields.split_first() else {
        out.push(Ok(Json::Object(partial)));
        return out.len() > MAX_STREAM_LEN;
    };
    for kr in Filter::filter(json, kf, globals, var_ctx) {
        let key = match kr {
            Err(e) => {
                out.push(Err(e));
                return true;
            }
            Ok(Json::String(s)) => s,
            Ok(other) => {
                out.push(Err(JQError::NonStringObjectKey(other)));
                return true;
            }
        };
        for vr in Filter::filter(json, vf, globals, var_ctx) {
            match vr {
                Err(e) => {
                    out.push(Err(e));
                    return true;
                }
                Ok(v) => {
                    let mut p = partial.clone();
                    p.push((key.clone(), v));
                    if build_object(rest, json, p, globals, var_ctx, out) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// The value a `catch` clause receives for a raised error. jq passes the
/// error's payload — the raw value for a user-raised `error`/`error(v)`, or a
/// string message for builtin (type) errors.
fn jqerror_to_json(e: &JQError) -> Json {
    match e {
        JQError::UserError(v) => v.clone(),
        other => Json::String(other.to_string()),
    }
}

/// Apply a binary operator to a single (left, right) value pair. Shared by the
/// tree-walking interpreter and the bytecode VM so the two engines cannot
/// diverge on operator semantics.
pub(crate) fn apply_binop(l: Json, r: Json, op: BinOp) -> Result<Json, JQError> {
    match op {
        BinOp::Add => add_json(l, r),
        BinOp::Sub => match (l, r) {
            (Json::Number(l), Json::Number(r)) => Ok(Json::Number(clamp_number(l - r))),
            (Json::Array(l), Json::Array(r)) => Ok(Json::Array(
                l.iter().filter(|x| !r.contains(x)).cloned().collect(),
            )),
            (l, r) => Err(JQError::BinOpTypeError(l, op, r)),
        },
        BinOp::Mul => match (l, r) {
            (Json::Number(l), Json::Number(r)) => Ok(Json::Number(clamp_number(l * r))),
            // String repetition (jq 1.7): a negative count yields null;
            // otherwise the count truncates (0 yields "").
            (Json::String(s), Json::Number(n)) | (Json::Number(n), Json::String(s)) => {
                if n < 0.0 {
                    Ok(Json::Null)
                } else {
                    let count = n.trunc() as usize;
                    // Guard against astronomical repetition.
                    match count.checked_mul(s.len()) {
                        Some(bytes) if bytes <= MAX_ALLOC_BYTES => Ok(Json::String(s.repeat(count))),
                        _ => Err(alloc_too_large()),
                    }
                }
            }
            // Object multiplication is recursive merge.
            (Json::Object(l), Json::Object(r)) => Ok(Json::Object(deep_merge(l, r))),
            (l, r) => Err(JQError::BinOpTypeError(l, op, r)),
        },
        BinOp::Div => match (l, r) {
            (Json::Number(l), Json::Number(r)) => {
                if r == 0.0 {
                    Err(JQError::DivisionByZero(Json::Number(l), Json::Number(r)))
                } else {
                    Ok(Json::Number(clamp_number(l / r)))
                }
            }
            // Dividing a string by a string splits it; splitting "" yields [].
            (Json::String(l), Json::String(r)) => {
                let parts: Vec<Json> = if l.is_empty() {
                    vec![]
                } else if r.is_empty() {
                    l.chars().map(|c| Json::String(c.to_string())).collect()
                } else {
                    l.split(r.as_str())
                        .map(|p| Json::String(p.to_string()))
                        .collect()
                };
                Ok(Json::Array(parts))
            }
            (l, r) => Err(JQError::BinOpTypeError(l, op, r)),
        },
        BinOp::Mod => match (l, r) {
            // jq truncates both operands to integers.
            (Json::Number(l), Json::Number(r)) => {
                let (li, ri) = (l.trunc() as i64, r.trunc() as i64);
                if ri == 0 {
                    Err(JQError::DivisionByZero(Json::Number(l), Json::Number(r)))
                } else {
                    Ok(Json::Number((li % ri) as f64))
                }
            }
            (l, r) => Err(JQError::BinOpTypeError(l, op, r)),
        },
        BinOp::Eq => Ok(Json::Boolean(l == r)),
        BinOp::Ne => Ok(Json::Boolean(l != r)),
        BinOp::Gt => Ok(Json::Boolean(l > r)),
        BinOp::Ge => Ok(Json::Boolean(l >= r)),
        BinOp::Lt => Ok(Json::Boolean(l < r)),
        BinOp::Le => Ok(Json::Boolean(l <= r)),
        BinOp::And => Ok(Json::Boolean(l.boolify() && r.boolify())),
        BinOp::Or => Ok(Json::Boolean(l.boolify() || r.boolify())),
    }
}

/// and the `add` builtin.
fn add_json(l: Json, r: Json) -> Result<Json, JQError> {
    match (l, r) {
        (Json::Null, r) => Ok(r),
        (l, Json::Null) => Ok(l),
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(clamp_number(l + r))),
        (Json::String(l), Json::String(r)) => Ok(Json::String(format!("{l}{r}"))),
        (Json::Array(l), Json::Array(r)) => Ok(Json::Array([l, r].concat())),
        (Json::Object(l), Json::Object(r)) => {
            let mut merged = l;
            for (k, v) in r {
                if let Some(slot) = merged.iter_mut().find(|(mk, _)| mk == &k) {
                    slot.1 = v;
                } else {
                    merged.push((k, v));
                }
            }
            Ok(Json::Object(merged))
        }
        (l, r) => Err(JQError::BinOpTypeError(l, BinOp::Add, r)),
    }
}

/// Recursive object merge for jq's `*` on objects: right wins, except two
/// objects merge recursively.
fn deep_merge(l: Vec<(String, Json)>, r: Vec<(String, Json)>) -> Vec<(String, Json)> {
    let mut merged = l;
    for (k, rv) in r {
        if let Some(slot) = merged.iter_mut().find(|(mk, _)| mk == &k) {
            let combined = match (slot.1.clone(), rv) {
                (Json::Object(lo), Json::Object(ro)) => Json::Object(deep_merge(lo, ro)),
                (_, rv) => rv,
            };
            slot.1 = combined;
        } else {
            merged.push((k, rv));
        }
    }
    merged
}

impl Filter {
    #[tracing::instrument(skip_all, ret)]
    pub fn filter(
        json: &Json,
        filter: &Filter,
        global_definitions: &HashMap<String, Filter>,
        variable_ctx: &mut HashMap<String, Filter>,
    ) -> Vec<Result<Json, JQError>> {
        tracing::debug!("Filtering with: {}", filter);
        tracing::trace!("JSON: {}", json);
        match filter {
            Filter::Dot => vec![Ok(json.clone())],
            // `EXP as $pat | BODY`: for each output of EXP, bind $pat and
            // evaluate BODY on the *original* input with that binding in
            // scope. The binding must scope BODY per value (jq semantics),
            // which is only possible where both the binding and its body are
            // visible — here, at the pipe. A bare `EXP as $pat` (no pipe) is
            // handled by the BindingExpression arm as `EXP as $pat | .`.
            Filter::Pipe(f1, f2) if matches!(f1.as_ref(), Filter::BindingExpression(_, _)) => {
                let Filter::BindingExpression(values, pat) = f1.as_ref() else {
                    unreachable!()
                };
                let mut vars = Vec::new();
                collect_pattern_vars(pat, &mut vars);
                let mut out = Vec::new();
                for result in Filter::filter(json, values, global_definitions, variable_ctx) {
                    match result {
                        Ok(v) => {
                            let saved: Vec<_> =
                                vars.iter().map(|n| variable_ctx.get(n).cloned()).collect();
                            destructure_pattern(&v, pat, variable_ctx);
                            out.extend(Filter::filter(json, f2, global_definitions, variable_ctx));
                            for (n, old) in vars.iter().zip(saved) {
                                match old {
                                    Some(f) => {
                                        variable_ctx.insert(n.clone(), f);
                                    }
                                    None => {
                                        variable_ctx.remove(n);
                                    }
                                }
                            }
                        }
                        Err(e) => out.push(Err(e)),
                    }
                    if out.len() > MAX_STREAM_LEN {
                        return vec![Err(alloc_too_large())];
                    }
                }
                out
            }
            Filter::Pipe(f1, f2) => {
                // Feed each of f1's outputs into f2. Errors from f1 must
                // propagate (not be silently dropped): `error | f` is an error,
                // and `try`/`?` rely on seeing it.
                let mut out = Vec::new();
                for result in Filter::filter(json, f1, global_definitions, variable_ctx) {
                    match result {
                        Ok(j) => {
                            out.extend(Filter::filter(&j, f2, global_definitions, variable_ctx))
                        }
                        Err(e) => out.push(Err(e)),
                    }
                    if out.len() > MAX_STREAM_LEN {
                        return vec![Err(alloc_too_large())];
                    }
                }
                out
            }
            Filter::Comma(f1, f2) => {
                let mut out = Filter::filter(json, f1, global_definitions, variable_ctx);
                out.extend(Filter::filter(json, f2, global_definitions, variable_ctx));
                if out.len() > MAX_STREAM_LEN {
                    return vec![Err(alloc_too_large())];
                }
                out
            }
            Filter::ObjIndex(s) => match json {
                Json::Object(obj) => {
                    let s = Filter::filter(json, s, global_definitions, variable_ctx);
                    s.into_iter()
                        .map(|s| {
                            if let Ok(Json::String(s)) = s {
                                Ok(obj
                                    .iter()
                                    .find(|(key, _)| key.as_str() == s.as_str())
                                    .map(|(_, value)| value.clone())
                                    .unwrap_or(Json::Null))
                            } else {
                                s
                            }
                        })
                        .collect::<Vec<_>>()
                }
                // jq's default semantics: field access on null yields null
                Json::Null => {
                    let s = Filter::filter(json, s, global_definitions, variable_ctx);
                    s.into_iter()
                        .map(|s| match s {
                            Ok(Json::String(_)) => Ok(Json::Null),
                            Ok(other) => Err(JQError::NonStringObjectKey(other)),
                            err => err,
                        })
                        .collect::<Vec<_>>()
                }
                _ => vec![Err(JQError::ObjIndexForNonObject(json.clone()))],
            },
            Filter::ArrayIndex(i) => {
                // `.[expr]` is jq's generic index: it dispatches on both the
                // input value and the index value. Arrays take integer indices
                // (negative counts from the end), objects take string keys,
                // and null indexes to null; every other pairing is an error.
                let indices = Filter::filter(json, i, global_definitions, variable_ctx);
                indices
                    .into_iter()
                    .map(|idx| {
                        let idx = match idx {
                            Ok(v) => v,
                            Err(e) => return Err(e),
                        };
                        match (json, &idx) {
                            (Json::Array(arr), Json::Number(n)) => {
                                if n.is_nan() || n.is_infinite() || n.fract() != 0.0 {
                                    return Err(JQError::InvalidArrayIndex(
                                        json.clone(),
                                        Json::Number(*n),
                                    ));
                                }
                                let len = arr.len() as i64;
                                let mut k = *n as i64;
                                if k < 0 {
                                    k += len;
                                }
                                if k < 0 || k >= len {
                                    Ok(Json::Null)
                                } else {
                                    Ok(arr[k as usize].clone())
                                }
                            }
                            (Json::Object(obj), Json::String(key)) => Ok(obj
                                .iter()
                                .find(|(k, _)| k.as_str() == key.as_str())
                                .map(|(_, v)| v.clone())
                                .unwrap_or(Json::Null)),
                            // jq's default semantics: indexing null yields null.
                            (Json::Null, Json::Number(_)) | (Json::Null, Json::String(_)) => {
                                Ok(Json::Null)
                            }
                            (Json::Array(_), other) => {
                                Err(JQError::InvalidArrayIndex(json.clone(), other.clone()))
                            }
                            (Json::Object(_), other) => {
                                Err(JQError::NonStringObjectKey(other.clone()))
                            }
                            _ => Err(JQError::ArrIndexForNonArray(json.clone())),
                        }
                    })
                    .collect::<Vec<_>>()
            }
            Filter::ArrayIterator => match json {
                Json::Array(arr) => arr.iter().map(|value| Ok(value.clone())).collect(),
                Json::Object(obj) => obj.iter().map(|(_, value)| Ok(value.clone())).collect(),
                _ => vec![Err(JQError::ArrIteratorForNonIterable(json.clone()))],
            },
            Filter::Null => vec![Ok(Json::Null)],
            Filter::Boolean(b) => vec![Ok(Json::Boolean(*b))],
            Filter::Number(n) => vec![Ok(Json::Number(*n))],
            Filter::String(s) => vec![Ok(Json::String(s.clone()))],
            Filter::Array(arr) => {
                let results = arr
                    .iter()
                    .flat_map(|f| Filter::filter(json, f, global_definitions, variable_ctx))
                    .take(MAX_STREAM_LEN + 1)
                    .collect::<Vec<_>>();
                if results.len() > MAX_STREAM_LEN {
                    return vec![Err(alloc_too_large())];
                }
                let (results, errs): (Vec<_>, Vec<_>) =
                    results.into_iter().partition(Result::is_ok);

                if errs.is_empty() {
                    vec![Ok(Json::Array(
                        results.into_iter().map(Result::unwrap).collect(),
                    ))]
                } else {
                    vec![errs[0].clone()]
                }
            }
            Filter::Object(obj) => {
                // jq nests fields left-to-right (first field outermost), so an
                // error in an earlier field surfaces even when a later field is
                // an empty stream; `build_object` matches that exactly.
                let mut out = Vec::new();
                build_object(
                    obj,
                    json,
                    Vec::new(),
                    global_definitions,
                    variable_ctx,
                    &mut out,
                );
                out
            }
            Filter::UnOp(un_op, f) => {
                let results = Filter::filter(json, f, global_definitions, variable_ctx);
                results
                    .into_iter()
                    .map(|result| match result {
                        Ok(json) => match un_op {
                            UnOp::Neg => match json {
                                Json::Number(n) => Ok(Json::Number(-n)),
                                _ => Err(JQError::UnOpTypeError(json, *un_op)),
                            },
                        },
                        Err(err) => Err(err),
                    })
                    .collect()
            }
            Filter::BinOp(l, bin_op @ (BinOp::And | BinOp::Or), r) => {
                // jq's and/or iterate the LEFT stream in the outer loop and
                // short-circuit per left value: a truthy `or` lhs (falsy
                // `and` lhs) emits without evaluating the right side at all
                // (`true or error` is `true`).
                let is_and = matches!(bin_op, BinOp::And);
                let ls = Filter::filter(json, l, global_definitions, variable_ctx);
                let mut out = vec![];
                for lres in ls {
                    match lres {
                        Err(e) => out.push(Err(e)),
                        Ok(lv) => {
                            let lt = lv.boolify();
                            if is_and && !lt {
                                out.push(Ok(Json::Boolean(false)));
                            } else if !is_and && lt {
                                out.push(Ok(Json::Boolean(true)));
                            } else {
                                for rres in
                                    Filter::filter(json, r, global_definitions, variable_ctx)
                                {
                                    match rres {
                                        Err(e) => out.push(Err(e)),
                                        Ok(rv) => out.push(Ok(Json::Boolean(rv.boolify()))),
                                    }
                                }
                            }
                        }
                    }
                    if out.len() > MAX_STREAM_LEN {
                        return vec![Err(alloc_too_large())];
                    }
                }
                out
            }
            Filter::BinOp(l, bin_op, r) => {
                let ls = Filter::filter(json, l, global_definitions, variable_ctx);
                let rs = Filter::filter(json, r, global_definitions, variable_ctx);

                // A binop over two streams is their cartesian product, so
                // `.[] op .[]` on a large array is quadratic (and nested
                // binops compound it). Guard the product size before
                // materializing it (this check is O(1)).
                if ls.len().saturating_mul(rs.len()) > MAX_STREAM_LEN {
                    return vec![Err(alloc_too_large())];
                }

                // jq iterates the right operand in the outer loop, the left in
                // the inner one. With an empty left stream the inner loop never
                // runs, so the right operand's Ok values produce nothing — but
                // its *errors* still surface (they are raised as the outer loop
                // pulls each value). `iproduct!` would drop them (no pair to
                // carry them), so handle the empty-left case explicitly.
                if ls.is_empty() {
                    return rs.into_iter().filter(Result::is_err).collect();
                }

                // jq iterates the right operand's stream in the outer loop:
                // (1,2) + (10,20) yields 11, 12, 21, 22
                itertools::iproduct!(rs, ls)
                    .map(|(r, l)| match (l, r) {
                        (Err(err), _) | (_, Err(err)) => Err(err),
                        (Ok(l), Ok(r)) => apply_binop(l, r, *bin_op),
                    })
                    .collect::<Vec<_>>()
            }
            Filter::Empty => vec![],
            // `error` raises an error carrying the input value; `error(v)` is
            // `v | error` (defs.jq), so this covers both.
            Filter::Error => vec![Err(JQError::UserError(json.clone()))],
            Filter::Call(name, filters_) => match filters_ {
                Some(args) => {
                    tracing::debug!("Calling filter: {name} with args: {:?}", args);
                    // 1-argument native builtins: evaluate the argument on the
                    // input (jq semantics), then apply, producing one output per
                    // argument output.
                    if args.len() == 1
                        && matches!(
                            name.as_str(),
                            "has" | "startswith" | "endswith" | "ltrimstr" | "rtrimstr"
                        )
                    {
                        let arg_vals =
                            Filter::filter(json, &args[0], global_definitions, variable_ctx);
                        let mut out = Vec::with_capacity(arg_vals.len());
                        for av in arg_vals {
                            match av {
                                Err(e) => out.push(Err(e)),
                                Ok(v) => out.push(apply_one_arg_string(name, json, &v)),
                            }
                        }
                        return out;
                    }
                    // Find the filter with the given name. Unknown arg'd builtins
                    // are a graceful error (not a panic).
                    let Some(filter) = global_definitions.get(name) else {
                        return vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))];
                    };
                    // The filter should have the same number of arguments as the number of arguments passed
                    if let Filter::Bound(params, filter) = filter {
                        if params.len() != args.len() {
                            return vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))];
                        }
                        let mut filter = *filter.clone();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            tracing::trace!("Substituting {} with {}", param, arg);
                            filter = filter.substitute(param, arg);
                            tracing::trace!("Substituted filter: {}", filter);
                        }

                        Filter::filter(json, &filter, global_definitions, variable_ctx)
                    } else {
                        vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))]
                    }
                }
                None => {
                    // Native builtins (jq implements these in C; they have no
                    // jq-level definition — docs/type-system-scope.md §9)
                    if name == "length" {
                        return vec![match json {
                            Json::Null => Ok(Json::Number(0.0)),
                            Json::Boolean(_) => Err(JQError::UnOpTypeError(
                                json.clone(),
                                UnOp::Neg, // closest existing variant: "has no length"
                            )),
                            Json::Number(n) => Ok(Json::Number(n.abs())),
                            Json::String(s) => Ok(Json::Number(s.chars().count() as f64)),
                            Json::Array(arr) => Ok(Json::Number(arr.len() as f64)),
                            Json::Object(obj) => Ok(Json::Number(obj.len() as f64)),
                        }];
                    }
                    if name == "keys" {
                        // Object keys come out sorted; array "keys" are the
                        // indices; everything else has no keys
                        return vec![match json {
                            Json::Object(obj) => {
                                let mut ks: Vec<String> =
                                    obj.iter().map(|(k, _)| k.clone()).collect();
                                ks.sort();
                                Ok(Json::Array(ks.into_iter().map(Json::String).collect()))
                            }
                            Json::Array(arr) => Ok(Json::Array(
                                (0..arr.len()).map(|i| Json::Number(i as f64)).collect(),
                            )),
                            other => Err(JQError::ObjIndexForNonObject(other.clone())),
                        }];
                    }
                    if name == "floor" {
                        return vec![match json {
                            Json::Number(n) => Ok(Json::Number(n.floor())),
                            other => Err(JQError::UnOpTypeError(other.clone(), UnOp::Neg)),
                        }];
                    }
                    if name == "tostring" {
                        // Strings pass through unquoted; everything else is
                        // compact JSON
                        return vec![Ok(Json::String(match json {
                            Json::String(s) => s.clone(),
                            other => other.to_compact_string(),
                        }))];
                    }
                    if name == "tonumber" {
                        return vec![match json {
                            Json::Number(n) => Ok(Json::Number(*n)),
                            Json::String(s) => match s.trim().parse::<f64>() {
                                Ok(n) if n.is_finite() => Ok(Json::Number(n)),
                                _ => Err(JQError::UnOpTypeError(json.clone(), UnOp::Neg)),
                            },
                            other => Err(JQError::UnOpTypeError(other.clone(), UnOp::Neg)),
                        }];
                    }
                    if name == "reverse" {
                        // jq 1.7 `reverse` is `[.[length-1-range(0;length)]]`:
                        // arrays reverse; null and the empty object yield []
                        // (range is empty, so no numeric index is taken); a
                        // non-empty object errors (object indexed by number);
                        // strings are not reversible in jq 1.7.
                        return vec![match json {
                            Json::Array(arr) => {
                                Ok(Json::Array(arr.iter().rev().cloned().collect()))
                            }
                            Json::Null => Ok(Json::Array(vec![])),
                            Json::Object(obj) if obj.is_empty() => Ok(Json::Array(vec![])),
                            Json::String(s) if s.is_empty() => Ok(Json::Array(vec![])),
                            // Any length-0 input reverses to []; a number's
                            // length is its absolute value, so only 0 / -0 has
                            // length 0 (any other number would index into
                            // itself and error).
                            Json::Number(n) if *n == 0.0 => Ok(Json::Array(vec![])),
                            other => Err(JQError::ArrIndexForNonArray(other.clone())),
                        }];
                    }
                    if name == "sort" {
                        return vec![match json {
                            Json::Array(arr) => {
                                let mut sorted = arr.clone();
                                sorted.sort();
                                Ok(Json::Array(sorted))
                            }
                            other => Err(JQError::ArrIndexForNonArray(other.clone())),
                        }];
                    }
                    if name == "min" || name == "max" {
                        return vec![match json {
                            Json::Array(arr) if arr.is_empty() => Ok(Json::Null),
                            Json::Array(arr) => {
                                let v = if name == "min" {
                                    arr.iter().min()
                                } else {
                                    arr.iter().max()
                                };
                                Ok(v.cloned().unwrap())
                            }
                            other => Err(JQError::ArrIndexForNonArray(other.clone())),
                        }];
                    }
                    if name == "add" {
                        // jq: `reduce .[] as $x (null; . + $x)`. `.[]` iterates
                        // array elements or object values, so numbers sum,
                        // strings/arrays concat, objects merge; empty yields
                        // null. Non-iterable inputs error.
                        let values: Vec<&Json> = match json {
                            Json::Array(arr) => arr.iter().collect(),
                            Json::Object(obj) => obj.iter().map(|(_, v)| v).collect(),
                            other => {
                                return vec![Err(JQError::ArrIndexForNonArray(other.clone()))]
                            }
                        };
                        let mut acc = Json::Null;
                        for item in values {
                            acc = match add_json(acc, item.clone()) {
                                Ok(v) => v,
                                Err(e) => return vec![Err(e)],
                            };
                        }
                        return vec![Ok(acc)];
                    }
                    if name == "flatten" {
                        // jq: `reduce .[] as $x ([]; if $x|type=="array" ...)`.
                        // `.[]` iterates array elements or object values; only
                        // nested *arrays* are recursed into (object values are
                        // kept as-is), to full depth. Non-iterable inputs error.
                        fn flat(arr: &[Json], out: &mut Vec<Json>) {
                            for v in arr {
                                match v {
                                    Json::Array(inner) => flat(inner, out),
                                    other => out.push(other.clone()),
                                }
                            }
                        }
                        let values: Vec<Json> = match json {
                            Json::Array(arr) => arr.clone(),
                            Json::Object(obj) => {
                                obj.iter().map(|(_, v)| v.clone()).collect()
                            }
                            other => {
                                return vec![Err(JQError::ArrIndexForNonArray(other.clone()))]
                            }
                        };
                        let mut out = vec![];
                        flat(&values, &mut out);
                        return vec![Ok(Json::Array(out))];
                    }
                    if name == "keys_unsorted" {
                        // Object keys in insertion order (objects now preserve
                        // it); array indices; everything else has no keys.
                        return vec![match json {
                            Json::Object(obj) => Ok(Json::Array(
                                obj.iter().map(|(k, _)| Json::String(k.clone())).collect(),
                            )),
                            Json::Array(arr) => Ok(Json::Array(
                                (0..arr.len()).map(|i| Json::Number(i as f64)).collect(),
                            )),
                            other => Err(JQError::ObjIndexForNonObject(other.clone())),
                        }];
                    }
                    if name == "tojson" {
                        // Compact JSON serialization (objects in insertion order).
                        return vec![Ok(Json::String(json.to_compact_string()))];
                    }
                    if name == "explode" {
                        // String → array of Unicode codepoints; errors otherwise.
                        return vec![match json {
                            Json::String(s) => Ok(Json::Array(
                                s.chars().map(|c| Json::Number(c as u32 as f64)).collect(),
                            )),
                            other => Err(JQError::UnOpTypeError(other.clone(), UnOp::Neg)),
                        }];
                    }
                    if name == "implode" {
                        // Array of codepoints → string; errors on bad element.
                        return vec![match json {
                            Json::Array(arr) => {
                                let mut s = String::new();
                                for v in arr {
                                    match v {
                                        Json::Number(n)
                                            if *n >= 0.0
                                                && n.fract() == 0.0
                                                && *n <= u32::MAX as f64 =>
                                        {
                                            match char::from_u32(*n as u32) {
                                                Some(c) => s.push(c),
                                                None => {
                                                    return vec![Err(JQError::UnOpTypeError(
                                                        json.clone(),
                                                        UnOp::Neg,
                                                    ))]
                                                }
                                            }
                                        }
                                        _ => {
                                            return vec![Err(JQError::UnOpTypeError(
                                                json.clone(),
                                                UnOp::Neg,
                                            ))]
                                        }
                                    }
                                }
                                Ok(Json::String(s))
                            }
                            other => Err(JQError::UnOpTypeError(other.clone(), UnOp::Neg)),
                        }];
                    }
                    if name == "ascii_downcase" || name == "ascii_upcase" {
                        let up = name == "ascii_upcase";
                        return vec![match json {
                            Json::String(s) => Ok(Json::String(
                                s.chars()
                                    .map(|c| {
                                        if up {
                                            c.to_ascii_uppercase()
                                        } else {
                                            c.to_ascii_lowercase()
                                        }
                                    })
                                    .collect(),
                            )),
                            other => Err(JQError::UnOpTypeError(other.clone(), UnOp::Neg)),
                        }];
                    }
                    let filter = global_definitions.get(name).ok_or_else(|| {
                        JQError::FilterNotDefined(
                            name.to_string(),
                            filters_.as_ref().map_or(0, |f| f.len()),
                        )
                    });
                    match filter {
                        Err(err) => vec![Err(err.clone())],
                        Ok(filter) => {
                            Filter::filter(json, filter, global_definitions, variable_ctx)
                        }
                    }
                }
            },
            Filter::IfThenElse(filter, filter1, filter2) => {
                // The condition is itself a stream; an error in it must
                // propagate (not be dropped), so `try`/`?` can see it.
                let mut out = Vec::new();
                for result in Filter::filter(json, filter, global_definitions, variable_ctx) {
                    match result {
                        Ok(cond) => {
                            // jq truthiness: everything except null and false.
                            let branch = if cond.boolify() { filter1 } else { filter2 };
                            out.extend(Filter::filter(
                                json,
                                branch,
                                global_definitions,
                                variable_ctx,
                            ));
                        }
                        Err(e) => out.push(Err(e)),
                    }
                    if out.len() > MAX_STREAM_LEN {
                        return vec![Err(alloc_too_large())];
                    }
                }
                out
            }
            Filter::Alternative(lhs, rhs) => {
                // jq `l // r`: emit `l`'s truthy outputs (drop null/false); if
                // `l` errors, the error propagates (// does not catch it),
                // keeping the truthy outputs emitted before it; only if `l`
                // finishes without emitting any truthy value do we run `r`.
                let mut out = Vec::new();
                let mut any_truthy = false;
                for result in Filter::filter(json, lhs, global_definitions, variable_ctx) {
                    match result {
                        Ok(v) => {
                            if v.boolify() {
                                any_truthy = true;
                                out.push(Ok(v));
                            }
                        }
                        Err(e) => {
                            out.push(Err(e));
                            return out;
                        }
                    }
                    if out.len() > MAX_STREAM_LEN {
                        return vec![Err(alloc_too_large())];
                    }
                }
                if !any_truthy {
                    out.extend(Filter::filter(json, rhs, global_definitions, variable_ctx));
                }
                out
            }
            Filter::Bound(items, filter) => {
                // for item in items {
                //     todo!()
                // }

                Filter::filter(json, filter, global_definitions, variable_ctx)
            }
            Filter::FunctionExpression(local_defs, expr) => {
                tracing::debug!(
                    "Function expression with local definitions: {:?}",
                    local_defs
                );
                let mut scoped_filters = global_definitions.clone();
                for (name, local_filter) in local_defs {
                    scoped_filters.insert(name.clone(), local_filter.clone());
                }
                // todo: check this for performance implications
                Filter::filter(json, expr, &scoped_filters, variable_ctx)
            }
            Filter::BindingExpression(lhs, pat) => {
                // A binding with no continuation is `lhs as $pat | .`: yield
                // the original input once per bound value. The binding scopes
                // nothing observable here, so restore the shadowed vars after
                // (a piped binding is handled by the specialized Pipe arm).
                let bind_vals = Filter::filter(json, lhs, global_definitions, variable_ctx);
                let mut vars = Vec::new();
                collect_pattern_vars(pat, &mut vars);
                let mut out = Vec::new();
                for res in bind_vals {
                    match res {
                        Ok(j) => {
                            let saved: Vec<_> =
                                vars.iter().map(|n| variable_ctx.get(n).cloned()).collect();
                            destructure_pattern(&j, pat, variable_ctx);
                            out.push(Ok(json.clone()));
                            for (n, old) in vars.iter().zip(saved) {
                                match old {
                                    Some(f) => {
                                        variable_ctx.insert(n.clone(), f);
                                    }
                                    None => {
                                        variable_ctx.remove(n);
                                    }
                                }
                            }
                        }
                        Err(e) => out.push(Err(e)),
                    }
                }
                out
            }
            Filter::Variable(name) => {
                if let Some(bound_f) = variable_ctx.get(name) {
                    let bound_clone = bound_f.clone();
                    Filter::filter(json, &bound_clone, global_definitions, variable_ctx)
                } else {
                    vec![Err(JQError::FilterNotDefined(name.clone(), 0))]
                }
            }
            Filter::ReduceExpression(var, gen, init, update) => {
                let mut gen_items = Vec::new();
                for r in Filter::filter(json, gen, global_definitions, variable_ctx) {
                    match r {
                        Ok(j) => gen_items.push(j),
                        Err(e) => return vec![Err(e)],
                    }
                }

                let init_results = Filter::filter(json, init, global_definitions, variable_ctx);
                let acc0 = match init_results.into_iter().find(|r| r.is_ok()) {
                    Some(Ok(v)) => v,
                    Some(Err(e)) => return vec![Err(e)],
                    None => return vec![Err(JQError::Unknown)],
                };

                let old_binding = variable_ctx.get(var).cloned();
                let restore = |ctx: &mut HashMap<String, Filter>| match &old_binding {
                    Some(prev) => {
                        ctx.insert(var.clone(), prev.clone());
                    }
                    None => {
                        ctx.remove(var);
                    }
                };

                let mut acc = acc0;
                for item in gen_items {
                    // bind $var to the generated item
                    variable_ctx.insert(var.clone(), Filter::from_json_const(&item));

                    // jq folds with the *last* value of the update stream; an
                    // error propagates, and an empty update makes the
                    // accumulator null (jq 1.7).
                    let upd_results =
                        Filter::filter(&acc, update, global_definitions, variable_ctx);
                    let mut next = Json::Null;
                    for r in upd_results {
                        match r {
                            Ok(v) => next = v,
                            Err(e) => {
                                restore(variable_ctx);
                                return vec![Err(e)];
                            }
                        }
                    }
                    acc = next;
                }

                restore(variable_ctx);
                vec![Ok(acc)]
            }

            Filter::ForeachExpression(var, gen, init, update, extract) => {
                // Like reduce, but emits at each step: for each generated $var,
                // the update stream advances the state, and for every update
                // output the extract (identity if omitted) is emitted. The
                // state threads as the last update output. The source is
                // processed *incrementally* — a source error surfaces after
                // the earlier items' emissions (jq raises it there), so it must
                // not be collected up front.
                let init_results = Filter::filter(json, init, global_definitions, variable_ctx);
                let mut acc = match init_results.into_iter().find(|r| r.is_ok()) {
                    Some(Ok(v)) => v,
                    Some(Err(e)) => return vec![Err(e)],
                    None => return vec![Err(JQError::Unknown)],
                };
                let gen_results = Filter::filter(json, gen, global_definitions, variable_ctx);

                let old_binding = variable_ctx.get(var).cloned();
                let restore = |ctx: &mut HashMap<String, Filter>| match &old_binding {
                    Some(prev) => {
                        ctx.insert(var.clone(), prev.clone());
                    }
                    None => {
                        ctx.remove(var);
                    }
                };

                let mut out = Vec::new();
                for src in gen_results {
                    let item = match src {
                        Err(e) => {
                            restore(variable_ctx);
                            out.push(Err(e));
                            return out;
                        }
                        Ok(j) => j,
                    };
                    variable_ctx.insert(var.clone(), Filter::from_json_const(&item));
                    for r in Filter::filter(&acc, update, global_definitions, variable_ctx) {
                        match r {
                            Err(e) => {
                                restore(variable_ctx);
                                out.push(Err(e));
                                return out;
                            }
                            Ok(state) => {
                                acc = state.clone();
                                match extract {
                                    Some(ex) => out.extend(Filter::filter(
                                        &state,
                                        ex,
                                        global_definitions,
                                        variable_ctx,
                                    )),
                                    None => out.push(Ok(state)),
                                }
                            }
                        }
                        if out.len() > MAX_STREAM_LEN {
                            restore(variable_ctx);
                            return vec![Err(alloc_too_large())];
                        }
                    }
                }
                restore(variable_ctx);
                out
            }

            Filter::TryCatch(body, handler) => {
                // jq: emit the body's outputs until (if) it raises an error, at
                // which point the stream stops. `try f catch g` runs g with the
                // error value as input; bare `try f` / `f?` yields nothing on
                // error. Errors after the first are unreachable in jq's lazy
                // model, so we drop the eager tail past the first error.
                let results = Filter::filter(json, body, global_definitions, variable_ctx);
                let mut out = Vec::new();
                for r in results {
                    match r {
                        Ok(v) => out.push(Ok(v)),
                        Err(e) => {
                            if let Some(handler) = handler {
                                let err_val = jqerror_to_json(&e);
                                out.extend(Filter::filter(
                                    &err_val,
                                    handler,
                                    global_definitions,
                                    variable_ctx,
                                ));
                            }
                            break;
                        }
                    }
                }
                out
            }

            Filter::SliceExpression(start, end) => {
                // Evaluate start and end expressions
                let start_val = start.as_ref().map(|s| {
                    Filter::filter(json, s, global_definitions, variable_ctx)
                        .into_iter()
                        .find_map(|r| r.ok())
                        .and_then(|j| match j {
                            Json::Number(n) => Some(n as isize),
                            _ => None,
                        })
                });
                let end_val = end.as_ref().map(|e| {
                    Filter::filter(json, e, global_definitions, variable_ctx)
                        .into_iter()
                        .find_map(|r| r.ok())
                        .and_then(|j| match j {
                            Json::Number(n) => Some(n as isize),
                            _ => None,
                        })
                });

                match json {
                    Json::Array(arr) => {
                        let len = arr.len() as isize;
                        // Normalize indices (handle negative indices)
                        let normalize = |idx: isize| -> usize {
                            if idx < 0 {
                                (len + idx).max(0) as usize
                            } else {
                                idx.min(len) as usize
                            }
                        };

                        let s = start_val.flatten().map(normalize).unwrap_or(0);
                        let e = end_val.flatten().map(normalize).unwrap_or(len as usize);

                        if s >= e || s >= arr.len() {
                            vec![Ok(Json::Array(vec![]))]
                        } else {
                            vec![Ok(Json::Array(arr[s..e.min(arr.len())].to_vec()))]
                        }
                    }
                    Json::String(str_val) => {
                        let len = str_val.len() as isize;
                        let normalize = |idx: isize| -> usize {
                            if idx < 0 {
                                (len + idx).max(0) as usize
                            } else {
                                idx.min(len) as usize
                            }
                        };

                        let s = start_val.flatten().map(normalize).unwrap_or(0);
                        let e = end_val.flatten().map(normalize).unwrap_or(len as usize);

                        if s >= e || s >= str_val.len() {
                            vec![Ok(Json::String(String::new()))]
                        } else {
                            vec![Ok(Json::String(
                                str_val[s..e.min(str_val.len())].to_string(),
                            ))]
                        }
                    }
                    // jq's default semantics: slicing null yields null.
                    Json::Null => vec![Ok(Json::Null)],
                    _ => vec![Err(JQError::ArrIndexForNonArray(json.clone()))],
                }
            }

            Filter::Hole => vec![Err(JQError::IncompleteProgram)],
        }
    }

    pub fn substitute(&self, var: &str, arg: &Filter) -> Filter {
        match self {
            Filter::Dot
            | Filter::ObjIndex(_)
            | Filter::ArrayIndex(_)
            | Filter::ArrayIterator
            | Filter::Null
            | Filter::Boolean(_)
            | Filter::Number(_)
            | Filter::String(_)
            | Filter::Empty
            | Filter::Error => self.clone(),
            Filter::Pipe(filter, filter1) => Filter::Pipe(
                Box::new(filter.substitute(var, arg)),
                Box::new(filter1.substitute(var, arg)),
            ),
            Filter::Alternative(filter, filter1) => Filter::Alternative(
                Box::new(filter.substitute(var, arg)),
                Box::new(filter1.substitute(var, arg)),
            ),
            Filter::Comma(filter, filter1) => Filter::Comma(
                Box::new(filter.substitute(var, arg)),
                Box::new(filter1.substitute(var, arg)),
            ),
            Filter::Array(filters) => Filter::Array(
                filters
                    .iter()
                    .map(|filter| filter.substitute(var, arg))
                    .collect(),
            ),
            Filter::Object(items) => Filter::Object(
                items
                    .iter()
                    .map(|(filter, filter1)| {
                        (filter.substitute(var, arg), filter1.substitute(var, arg))
                    })
                    .collect(),
            ),
            Filter::UnOp(un_op, filter) => {
                Filter::UnOp(*un_op, Box::new(filter.substitute(var, arg)))
            }
            Filter::BinOp(filter, bin_op, filter1) => Filter::BinOp(
                Box::new(filter.substitute(var, arg)),
                *bin_op,
                Box::new(filter1.substitute(var, arg)),
            ),
            Filter::Call(name, filters) => {
                if name == var {
                    arg.clone()
                } else {
                    Filter::Call(
                        name.clone(),
                        filters.as_ref().map(|filters| {
                            filters
                                .iter()
                                .map(|filter| filter.substitute(var, arg))
                                .collect()
                        }),
                    )
                }
            }
            Filter::FunctionExpression(local_defs, expr) => {
                if local_defs.contains_key(var) {
                    self.clone()
                } else {
                    let new_local_defs = local_defs
                        .iter()
                        .map(|(name, filter)| (name.clone(), filter.substitute(var, arg)))
                        .collect();
                    Filter::FunctionExpression(new_local_defs, Box::new(expr.substitute(var, arg)))
                }
            }
            Filter::IfThenElse(filter, filter1, filter2) => Filter::IfThenElse(
                Box::new(filter.substitute(var, arg)),
                Box::new(filter1.substitute(var, arg)),
                Box::new(filter2.substitute(var, arg)),
            ),
            Filter::Bound(items, filter) => {
                if items.contains(&var.to_string()) {
                    self.clone()
                } else {
                    Filter::Bound(items.clone(), Box::new(filter.substitute(var, arg)))
                }
            }
            Filter::BindingExpression(lhs, pat) => {
                if pattern_binds_variable(pat, var) {
                    // If the pattern binds a variable with the same name as `var`,
                    // don't substitute to avoid shadowing
                    self.clone()
                } else {
                    // Substitute in both the left-hand side and the pattern
                    Filter::BindingExpression(
                        Box::new(lhs.substitute(var, arg)),
                        Box::new(pat.substitute(var, arg)),
                    )
                }
            }
            Filter::Variable(name) => {
                if name == var {
                    // If this variable matches the one being substituted, replace it with the argument
                    arg.clone()
                } else {
                    // Otherwise, keep it unchanged (it's a different variable)
                    self.clone()
                }
            }
            Filter::ReduceExpression(var, gen, init, upd) => Filter::ReduceExpression(
                var.clone(),
                Box::new(gen.substitute(var, arg)),
                Box::new(init.substitute(var, arg)),
                Box::new(upd.substitute(var, arg)),
            ),
            // `fvar` is the foreach's bind variable ($var namespace); the
            // substituted `var` is a function parameter, a different namespace.
            Filter::ForeachExpression(fvar, gen, init, upd, extract) => {
                Filter::ForeachExpression(
                    fvar.clone(),
                    Box::new(gen.substitute(var, arg)),
                    Box::new(init.substitute(var, arg)),
                    Box::new(upd.substitute(var, arg)),
                    extract.as_ref().map(|e| Box::new(e.substitute(var, arg))),
                )
            }
            Filter::SliceExpression(start, end) => Filter::SliceExpression(
                start.as_ref().map(|s| Box::new(s.substitute(var, arg))),
                end.as_ref().map(|e| Box::new(e.substitute(var, arg))),
            ),
            Filter::TryCatch(body, handler) => Filter::TryCatch(
                Box::new(body.substitute(var, arg)),
                handler
                    .as_ref()
                    .map(|h| Box::new(h.substitute(var, arg))),
            ),

            Filter::Hole => todo!(),
        }
    }
    fn from_json_const(j: &Json) -> Filter {
        match j {
            Json::Null => Filter::Null,
            Json::Boolean(b) => Filter::Boolean(*b),
            Json::Number(n) => Filter::Number(*n),
            Json::String(s) => Filter::String(s.clone()),
            Json::Array(a) => Filter::Array(a.iter().map(Self::from_json_const).collect()),
            Json::Object(o) => Filter::Object(
                o.iter()
                    .map(|(k, v)| (Filter::String(k.clone()), Self::from_json_const(v)))
                    .collect(),
            ),
        }
    }

    pub fn is_const_computable(&self) -> bool {
        match self {
            Filter::Dot => false,
            // The right side of the pipe only sees the output of lhs, so if left is const computable right is too
            Filter::Pipe(lhs, _) => lhs.is_const_computable(),
            Filter::Comma(lhs, rhs) => lhs.is_const_computable() && rhs.is_const_computable(),
            Filter::ObjIndex(f) | Filter::ArrayIndex(f) => false,
            Filter::ArrayIterator => false,
            Filter::Null | Filter::Boolean(_) | Filter::Number(_) | Filter::String(_) => true,
            Filter::Array(filters) => filters.iter().all(|f| f.is_const_computable()),
            Filter::Object(items) => items.iter().all(|(_, f)| f.is_const_computable()),
            Filter::UnOp(_, f) => f.is_const_computable(),
            Filter::BinOp(lhs, _, rhs) => lhs.is_const_computable() && rhs.is_const_computable(),
            Filter::Empty => true,
            Filter::Error => true,
            Filter::IfThenElse(if_, then, else_) => {
                if_.is_const_computable()
                    && then.is_const_computable()
                    && else_.is_const_computable()
            }
            Filter::Hole => false,
            // todo@alp: implement const computability for all filters, remove this catchall
            _ => false,
        }
    }
}

impl From<&str> for Filter {
    fn from(s: &str) -> Self {
        Filter::String(s.to_string())
    }
}

#[cfg(test)]
mod tests {

    use std::{collections::HashMap, vec};

    use tracing_subscriber::EnvFilter;

    use crate::{filters, parse, Filter, Json};

    fn builtin_filters() -> HashMap<String, Filter> {
        // read defs.jq
        tracing::debug!("Parsing built-in filters");
        filters(include_str!("../tjq/defs.jq"))
    }

    fn json(s: &str) -> Json {
        let sjson: serde_json::Value = serde_json::from_str(s).unwrap();
        fn sjson_to_json(sjson: &serde_json::Value) -> Json {
            match sjson {
                serde_json::Value::Null => Json::Null,
                serde_json::Value::Bool(b) => Json::Boolean(*b),
                serde_json::Value::Number(n) => Json::Number(n.as_f64().unwrap()),
                serde_json::Value::String(s) => Json::String(s.clone()),
                serde_json::Value::Array(arr) => {
                    Json::Array(arr.iter().map(sjson_to_json).collect())
                }
                serde_json::Value::Object(obj) => Json::Object(
                    obj.iter()
                        .map(|(k, v)| (k.clone(), sjson_to_json(v)))
                        .collect(),
                ),
            }
        }
        sjson_to_json(&sjson)
    }

    fn filter(s: &str) -> Filter {
        let (defs, filter) = parse(s);
        let filter = (&filter).into();
        tracing::debug!("Parsed filter: {}", filter);

        if defs.is_empty() {
            tracing::debug!("No local definitions, returning filter directly");
            return filter;
        }

        let defs: HashMap<String, Filter> = defs
            .into_iter()
            .map(|(name, f)| (name, (&f).into()))
            .collect();

        tracing::debug!("Local definitions: {:?}", defs.keys());

        Filter::FunctionExpression(defs, Box::new(filter))
    }

    fn run(f: &Filter, input: Json) -> Vec<Json> {
        let result = Filter::filter(&input, f, &builtin_filters(), &mut Default::default());
        result
            .into_iter()
            .filter_map(|res| match res {
                Ok(json) => Some(json),
                Err(err) => {
                    eprintln!("Error: {}", err);
                    None
                }
            })
            .collect()
    }

    /// Like `run`, but keeps the error/value distinction: returns `Ok` values
    /// as `Some(json)` and errors as `None`, in stream order. Lets tests assert
    /// on error propagation and empty streams.
    fn run_raw(src: &str, input: &str) -> Vec<Option<Json>> {
        let f = filter(src);
        Filter::filter(&json(input), &f, &builtin_filters(), &mut Default::default())
            .into_iter()
            .map(|r| r.ok())
            .collect()
    }

    #[test]
    fn test_pipe_propagates_errors() {
        // Regression: a left-operand error must not be silently dropped.
        assert_eq!(run_raw("error | 5", "null"), vec![None]);
        assert_eq!(run_raw(".x | 0.25", "0"), vec![None]);
        // A clean pipe still passes values through.
        assert_eq!(run_raw("1 | . + 1", "null"), vec![Some(json("2.0"))]);
    }

    #[test]
    fn test_generic_index() {
        // `.[expr]` dispatches on input and index type.
        assert_eq!(run_raw(".[\"a\"]", "{\"a\":1}"), vec![Some(json("1.0"))]);
        assert_eq!(run_raw(".[1]", "[10,20,30]"), vec![Some(json("20.0"))]);
        assert_eq!(run_raw(".[-1]", "[10,20,30]"), vec![Some(json("30.0"))]);
        // Type mismatches error rather than returning the index.
        assert_eq!(run_raw(".[\"k\"]", "[1]"), vec![None]);
        assert_eq!(run_raw(".[0]", "{\"a\":1}"), vec![None]);
        // null indexes to null.
        assert_eq!(run_raw(".[0]", "null"), vec![Some(json("null"))]);
    }

    #[test]
    fn test_object_construction_propagates_errors() {
        // Regression: a field whose value errors must surface the error, not
        // some other field's value. `reverse` errors on a non-empty string.
        assert_eq!(run_raw("{b: 5, k: reverse}", "\"a\""), vec![None]);
        // No error: the object is built.
        assert_eq!(
            run_raw("{b: 5}", "\"a\""),
            vec![Some(json("{\"b\":5}"))]
        );
    }

    #[test]
    fn test_reverse_length_zero() {
        // Any length-0 input reverses to []; only the number 0 qualifies.
        assert_eq!(run_raw("reverse", "0"), vec![Some(json("[]"))]);
        assert_eq!(run_raw("reverse", "\"\""), vec![Some(json("[]"))]);
        assert_eq!(run_raw("reverse", "5"), vec![None]); // errors
    }

    #[test]
    fn test_try_catch_optional() {
        // `f?` suppresses errors (empty stream); `try f catch g` runs g.
        assert_eq!(run_raw(".a?", "5"), Vec::<Option<Json>>::new());
        assert_eq!(run_raw(".a?", "{\"a\":1}"), vec![Some(json("1.0"))]);
        assert_eq!(
            run_raw("try error catch \"c\"", "null"),
            vec![Some(json("\"c\""))]
        );
        // Stream stops at the first error, keeping earlier outputs.
        assert_eq!(
            run_raw("[try (1,2,error,3) catch \"c\"]", "null"),
            vec![Some(json("[1,2,\"c\"]"))]
        );
    }

    #[test]
    fn test_if_propagates_condition_error() {
        // Regression: an error in the condition must propagate, not vanish.
        assert_eq!(run_raw("if error then 1 else 2 end", "null"), vec![None]);
        // `flatten` errors on null; wrapped in try, the error is caught.
        assert_eq!(
            run_raw("try (if flatten then 1 else 2 end) catch \"c\"", "null"),
            vec![Some(json("\"c\""))]
        );
    }

    #[test]
    fn test_catch_does_not_panic_on_any_error() {
        // Regression: `catch` stringifies the error via Display, which must be
        // total (no `todo!()`). Indexing an object with a number raises
        // NonStringObjectKey, whose Display was previously unimplemented.
        assert_eq!(
            run_raw("try .[0] catch \"c\"", "{\"a\":1}"),
            vec![Some(json("\"c\""))]
        );
    }

    #[test]
    fn test_binding_scopes_per_value() {
        // Regression: a stream binding must evaluate the body once per bound
        // value (previously a shared ctx leaked the last value to all).
        assert_eq!(
            run_raw("(1,2,3) as $x | $x", "null"),
            vec![Some(json("1")), Some(json("2")), Some(json("3"))]
        );
        // Binding a composite value must not panic (was todo!()).
        assert_eq!(run_raw(". as $x | $x", "[1,2]"), vec![Some(json("[1,2]"))]);
        // Nested bindings.
        assert_eq!(
            run_raw("(1,2) as $x | (10,20) as $y | $x + $y", "null"),
            vec![
                Some(json("11")),
                Some(json("21")),
                Some(json("12")),
                Some(json("22"))
            ]
        );
    }

    #[test]
    fn test_object_stream_keeps_values_before_error() {
        // Object construction produces one object per combination; jq raises a
        // field error at that combination, so valid objects produced *before*
        // it survive (visible under `?`).
        assert_eq!(
            run_raw("[{k:(1,error,3)}?]", "null"),
            vec![Some(json("[{\"k\":1}]"))]
        );
        // The full product order is preserved.
        assert_eq!(
            run_raw("{a:(1,2), b:(3,4)}", "null"),
            vec![
                Some(json("{\"a\":1,\"b\":3}")),
                Some(json("{\"a\":1,\"b\":4}")),
                Some(json("{\"a\":2,\"b\":3}")),
                Some(json("{\"a\":2,\"b\":4}")),
            ]
        );
    }

    #[test]
    fn test_binop_empty_left_surfaces_right_error() {
        // jq iterates the right operand outer, so with an empty left stream the
        // right operand's error still surfaces (`iproduct` would drop it).
        assert_eq!(run_raw(".[] * (1 + \"a\")", "[]"), vec![None]);
        // But an empty *right* stream never evaluates the left: no output.
        assert_eq!(run_raw("(1 + \"a\") * .[]", "[]"), Vec::<Option<Json>>::new());
    }

    #[test]
    fn test_reduce_semantics() {
        // Folds with the *last* update value (regression: was first).
        assert_eq!(
            run_raw("reduce .[] as $x (0; . + $x, . - $x)", "[1,2,3]"),
            vec![Some(json("-6"))]
        );
        // Empty update makes the accumulator null (jq 1.7), not an error.
        assert_eq!(
            run_raw("reduce .[] as $x (0; empty)", "[1,2,3]"),
            vec![Some(json("null"))]
        );
        assert_eq!(
            run_raw("reduce .[] as $x (0; . + $x)", "[1,2,3]"),
            vec![Some(json("6"))]
        );
    }

    #[test]
    fn test_foreach_semantics() {
        // Emits at each step (running sum); extract defaults to identity.
        assert_eq!(
            run_raw("foreach .[] as $x (0; . + $x)", "[1,2,3]"),
            vec![Some(json("1")), Some(json("3")), Some(json("6"))]
        );
        // With an explicit extract.
        assert_eq!(
            run_raw("foreach .[] as $x (0; . + $x; . * 2)", "[1,2,3]"),
            vec![Some(json("2")), Some(json("6")), Some(json("12"))]
        );
    }

    #[test]
    fn test_slice_null() {
        // jq: slicing null yields null.
        assert_eq!(run_raw(".[1:3]", "null"), vec![Some(json("null"))]);
        assert_eq!(run_raw(".[1:3]", "[1,2,3,4,5]"), vec![Some(json("[2,3]"))]);
    }

    #[test]
    fn test_error_carries_value() {
        // `error` raises an error carrying the input; `catch` receives it.
        assert_eq!(
            run_raw("try error catch .", "\"hello\""),
            vec![Some(json("\"hello\""))]
        );
        assert_eq!(
            run_raw("try error catch .", "{\"a\":1}"),
            vec![Some(json("{\"a\":1}"))]
        );
        // `error(v)` == `v | error`.
        assert_eq!(
            run_raw("try error(\"boom\") catch .", "null"),
            vec![Some(json("\"boom\""))]
        );
    }

    #[test]
    fn test_string_interpolation() {
        // `"a\(e)b"` desugars to `"a" + (e|tostring) + "b"`.
        assert_eq!(run_raw("\"x=\\(.x)\"", "{\"x\":5}"), vec![Some(json("\"x=5\""))]);
        assert_eq!(run_raw("\"\\(1+1)\"", "null"), vec![Some(json("\"2\""))]);
        assert_eq!(run_raw("\"\\([1,2])\"", "null"), vec![Some(json("\"[1,2]\""))]);
        // A stream interpolation is cartesian.
        assert_eq!(
            run_raw("\"\\(1,2)!\"", "null"),
            vec![Some(json("\"1!\"")), Some(json("\"2!\""))]
        );
    }

    #[test]
    fn test_postfix_iterate() {
        // `EXPR[]` iterates over EXPR's result (`.a[]` == `.a | .[]`).
        assert_eq!(
            run_raw(".a[]", "{\"a\":[1,2,3]}"),
            vec![Some(json("1")), Some(json("2")), Some(json("3"))]
        );
        assert_eq!(
            run_raw("[5,5][]", "null"),
            vec![Some(json("5")), Some(json("5"))]
        );
        // Bare `.[]` still iterates the input.
        assert_eq!(
            run_raw(".[]", "[7,8]"),
            vec![Some(json("7")), Some(json("8"))]
        );
    }

    #[test]
    fn test_foreach_source_error_incremental() {
        // The source is processed incrementally: `.y` on a number errors, but
        // the earlier `null` still emits its extract before the error.
        assert_eq!(
            run_raw("foreach (null, .y) as $y (-2; $y; length)", "0"),
            vec![Some(json("0")), None]
        );
    }

    #[test]
    fn test_object_field_error_ordering() {
        // The first field is the outer loop: an error in an earlier field
        // surfaces even when a later field is an empty stream, and objects
        // produced before an error survive (jq semantics).
        assert_eq!(run_raw("{a: error, b: empty}", "null"), vec![None]);
        assert_eq!(
            run_raw("{a: empty, b: error}", "null"),
            Vec::<Option<Json>>::new()
        );
        assert_eq!(run_raw("{a: (1,error), b: empty}", "null"), vec![None]);
        // Objects before the error, then the error.
        assert_eq!(
            run_raw("{a: (1,error,2), b: (3,4)}", "null"),
            vec![
                Some(json("{\"a\":1,\"b\":3}")),
                Some(json("{\"a\":1,\"b\":4}")),
                None
            ]
        );
    }

    #[test]
    fn test_object_shorthand() {
        // `{a}` == `{a: .a}`, `{$x}` == `{x: $x}`, `{"a"}` == `{"a": .a}`.
        assert_eq!(
            run_raw("{a,b}", "{\"a\":1,\"b\":2,\"c\":3}"),
            vec![Some(json("{\"a\":1,\"b\":2}"))]
        );
        assert_eq!(
            run_raw("{\"a\"}", "{\"a\":9}"),
            vec![Some(json("{\"a\":9}"))]
        );
    }

    #[test]
    fn test_plus() {
        let input = json("1.0");
        let f = filter("1 + 2");

        let results = run(&f, input);

        assert_eq!(results, vec![json("3.0")]);
    }

    #[test]
    fn test_minus() {
        let input = json("1.0");
        let f = filter("1 - 2");

        let results = run(&f, input);

        assert_eq!(results, vec![json("-1.0")]);
    }

    #[test]
    fn test_abs() {
        let input = json("-1.0");
        let f = filter("abs");

        let results = run(&f, input);

        assert_eq!(results, vec![json("1.0")]);
    }

    #[test]
    fn test_isboolean1() {
        let input = json("true");
        let f = filter("isboolean");

        let results = run(&f, input);

        assert_eq!(results, vec![json("true")]);

        let input = json("false");

        let results = run(&f, input);

        assert_eq!(results, vec![json("true")]);
    }

    #[test]
    fn test_isboolean2() {
        let input = json("1.0");
        let f = filter("isboolean");

        let results = run(&f, input);

        assert_eq!(results, vec![json("false")]);
    }

    #[test]
    fn test_type() {
        let input = json("null");
        let f = filter("type");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"null\"")]);

        let input = json("true");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"boolean\"")]);

        let input = json("1.0");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"number\"")]);

        let input = json("\"abc\"");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"string\"")]);

        let input = json("[]");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"array\"")]);

        let input = json("{}");

        let results = run(&f, input);

        assert_eq!(results, vec![json("\"object\"")]);
    }

    #[test]
    fn test_map1() {
        let input = json("[-1.0, 2.0]");
        let f = filter("map(abs)");

        let results = run(&f, input);

        assert_eq!(results, vec![json("[1.0, 2.0]")]);
    }

    #[test]
    fn test_map2() {
        let input = json("[-1.0, 2.0]");
        let f = filter("map(. * 2)");

        let results = run(&f, input);

        assert_eq!(results, vec![json("[-2.0, 4.0]")]);
    }

    #[test]
    fn test_map3() {
        let input = json("[[-1.0, -2.0], [3.0, 4.0]]");
        let f = filter("map(map(abs))");

        let results = run(&f, input);

        assert_eq!(results, vec![json("[[1.0, 2.0], [3.0, 4.0]]")]);
    }

    #[test]
    fn test_interpret_function_definition() {
        let input = json("null");
        let f = filter(
            r#"
        (def main:
            def add(a; b): a + b;
            def sub(a; b): a - b;
            def mul(a; b): a * b;
            add(1; 2);
        main)
        "#,
        );

        let results = run(&f, input);
        assert_eq!(
            results,
            vec![json("3.0")],
            "Expected 3.0, got: {:?}",
            results
        );
    }

    #[test]
    fn test_fibonacci() {
        let input = json("null");
        let f = filter(
            r#"
        (def fib(n):
            if n <= 1 then n else fib(n - 1) + fib(n - 2) end;
        fib(5))
        "#,
        );

        let results = run(&f, input);
        assert_eq!(results, vec![json("5.0")]);
    }
}
