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
    SliceExpression(Option<Box<Filter>>, Option<Box<Filter>>), // .[start:end], .[start:], .[:end]
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
        Filter::Variable(name) => {
            let lit = match val {
                Json::Null => Filter::Null,
                Json::Boolean(b) => Filter::Boolean(*b),
                Json::Number(n) => Filter::Number(*n),
                Json::String(s) => Filter::String(s.clone()),
                Json::Array(_) => todo!(),
                Json::Object(_) => todo!(),
            };
            variable_ctx.insert(name.clone(), lit);
        }
        Filter::Array(pats) => todo!(), //match these patterns

        Filter::Object(pairs) => todo!(),

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
fn clamp_number(n: f64) -> f64 {
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
            Filter::Pipe(f1, f2) => Filter::filter(json, f1, global_definitions, variable_ctx)
                .into_iter()
                .flat_map(|result| {
                    result.map(|json| Filter::filter(&json, f2, global_definitions, variable_ctx))
                })
                .flatten()
                .collect::<Vec<_>>(),
            Filter::Comma(f1, f2) => [
                Filter::filter(json, f1, global_definitions, variable_ctx),
                Filter::filter(json, f2, global_definitions, variable_ctx),
            ]
            .concat(),
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
            Filter::ArrayIndex(i) => match json {
                Json::Array(arr) => {
                    let i = Filter::filter(json, i, global_definitions, variable_ctx);
                    i.into_iter()
                        .map(|i| {
                            if let Ok(Json::Number(i)) = i {
                                if i.is_nan() || i.is_infinite() || i.fract() != 0.0 {
                                    return Err(JQError::InvalidArrayIndex(
                                        json.clone(),
                                        Json::Number(i),
                                    ));
                                }

                                Ok(arr.get(i as usize).cloned().unwrap_or(Json::Null))
                            } else {
                                i
                            }
                        })
                        .collect::<Vec<_>>()
                }
                // jq's default semantics: array index on null yields null
                Json::Null => {
                    let i = Filter::filter(json, i, global_definitions, variable_ctx);
                    i.into_iter()
                        .map(|i| match i {
                            Ok(Json::Number(_)) => Ok(Json::Null),
                            Ok(other) => Err(JQError::InvalidArrayIndex(json.clone(), other)),
                            err => err,
                        })
                        .collect::<Vec<_>>()
                }
                _ => vec![Err(JQError::ArrIndexForNonArray(json.clone()))],
            },
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
                    .collect::<Vec<_>>();
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
                let results: Vec<(Vec<Result<Json, JQError>>, Vec<Result<Json, JQError>>)> = obj
                    .iter()
                    .map(|(f1, f2)| {
                        (
                            Filter::filter(json, f1, global_definitions, variable_ctx),
                            Filter::filter(json, f2, global_definitions, variable_ctx),
                        )
                    })
                    .collect();

                let results = results
                    .into_iter()
                    .map(|(keys, values)| itertools::iproduct!(keys, values).collect::<Vec<_>>())
                    .multi_cartesian_product()
                    .collect::<Vec<_>>();

                let (results, errs): (Vec<_>, Vec<_>) = results
                    .into_iter()
                    .partition(|results| results.iter().all(|(k, v)| k.is_ok() && v.is_ok()));

                if errs.is_empty() {
                    let objs: Vec<Vec<(Json, Json)>> = results
                        .into_iter()
                        .map(|results| {
                            results
                                .into_iter()
                                .map(|(k, v)| (k.unwrap(), v.unwrap()))
                                .collect()
                        })
                        .collect();

                    let err = objs
                        .iter()
                        .find(|obj| obj.iter().any(|(k, _)| !matches!(k, Json::String(_))));

                    if let Some(obj) = err {
                        vec![Err(JQError::NonStringObjectKey(obj[0].0.clone()))]
                    } else {
                        objs.into_iter()
                            .map(|obj| {
                                Ok(Json::Object(
                                    obj.into_iter()
                                        .map(|(k, v)| {
                                            // Keys are verified strings above;
                                            // Display would add quotes around
                                            // the key text
                                            let key = match k {
                                                Json::String(s) => s,
                                                _ => unreachable!("checked above"),
                                            };
                                            (key, v)
                                        })
                                        .collect(),
                                ))
                            })
                            .collect()
                    }
                } else {
                    let (k, v) = errs[0][0].clone();
                    if k.is_err() {
                        vec![k]
                    } else {
                        vec![v]
                    }
                }
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
                }
                out
            }
            Filter::BinOp(l, bin_op, r) => {
                let ls = Filter::filter(json, l, global_definitions, variable_ctx);
                let rs = Filter::filter(json, r, global_definitions, variable_ctx);

                // jq iterates the right operand's stream in the outer loop:
                // (1,2) + (10,20) yields 11, 12, 21, 22
                itertools::iproduct!(rs, ls)
                    .map(|(r, l)| match (l, r) {
                        (Err(err), _) | (_, Err(err)) => Err(err),
                        (Ok(l), Ok(r)) => match bin_op {
                            BinOp::Add => add_json(l, r),
                            BinOp::Sub => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => {
                                    Ok(Json::Number(clamp_number(l - r)))
                                }
                                (Json::Array(l), Json::Array(r)) => Ok(Json::Array(
                                    l.iter().filter(|x| !r.contains(x)).cloned().collect(),
                                )),
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mul => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => {
                                    Ok(Json::Number(clamp_number(l * r)))
                                }
                                // String repetition (jq 1.7): a negative count
                                // yields null; otherwise the count truncates
                                // (0 yields "")
                                (Json::String(s), Json::Number(n))
                                | (Json::Number(n), Json::String(s)) => {
                                    if n < 0.0 {
                                        Ok(Json::Null)
                                    } else {
                                        let count = n.trunc() as usize;
                                        // Guard against astronomical repetition
                                        // (an extreme count from data would
                                        // otherwise attempt a petabyte
                                        // allocation and abort the process,
                                        // uncatchable by catch_unwind). jq
                                        // effectively hangs on such inputs and
                                        // is killed by the harness timeout, so
                                        // it never produces one to compare
                                        // against.
                                        match count.checked_mul(s.len()) {
                                            Some(bytes) if bytes <= MAX_ALLOC_BYTES => {
                                                Ok(Json::String(s.repeat(count)))
                                            }
                                            _ => Err(JQError::AllocationTooLarge),
                                        }
                                    }
                                }
                                // Object multiplication is recursive merge
                                (Json::Object(l), Json::Object(r)) => {
                                    Ok(Json::Object(deep_merge(l, r)))
                                }
                                // jq has no array repetition: `[1] * 2` is a
                                // type error
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Div => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => {
                                    if r == 0.0 {
                                        Err(JQError::DivisionByZero(
                                            Json::Number(l),
                                            Json::Number(r),
                                        ))
                                    } else {
                                        Ok(Json::Number(clamp_number(l / r)))
                                    }
                                }
                                // Dividing a string by a string splits it;
                                // splitting the empty string yields [] in jq
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
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mod => match (l, r) {
                                // jq truncates both operands to integers
                                (Json::Number(l), Json::Number(r)) => {
                                    let (li, ri) = (l.trunc() as i64, r.trunc() as i64);
                                    if ri == 0 {
                                        Err(JQError::DivisionByZero(
                                            Json::Number(l),
                                            Json::Number(r),
                                        ))
                                    } else {
                                        Ok(Json::Number((li % ri) as f64))
                                    }
                                }
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Eq => Ok(Json::Boolean(l == r)),
                            BinOp::Ne => Ok(Json::Boolean(l != r)),
                            BinOp::Gt => Ok(Json::Boolean(l > r)),
                            BinOp::Ge => Ok(Json::Boolean(l >= r)),
                            BinOp::Lt => Ok(Json::Boolean(l < r)),
                            BinOp::Le => Ok(Json::Boolean(l <= r)),
                            BinOp::And => Ok(Json::Boolean(l.boolify() && r.boolify())),
                            BinOp::Or => Ok(Json::Boolean(l.boolify() || r.boolify())),
                        },
                    })
                    .collect::<Vec<_>>()
            }
            Filter::Empty => vec![],
            Filter::Error => vec![Err(JQError::Unknown)],
            Filter::Call(name, filters_) => match filters_ {
                Some(args) => {
                    tracing::debug!("Calling filter: {name} with args: {:?}", args);
                    // Find the filter with the given name
                    let filter = global_definitions
                        .get(name)
                        .unwrap_or_else(|| panic!("Filter '{name}' not found"));
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
                let results = Filter::filter(json, filter, global_definitions, variable_ctx);
                results
                    .into_iter()
                    .flat_map(|result| {
                        result.map(|json_| {
                            // jq truthiness: everything except null and false
                            if json_.boolify() {
                                Filter::filter(json, filter1, global_definitions, variable_ctx)
                            } else {
                                Filter::filter(json, filter2, global_definitions, variable_ctx)
                            }
                        })
                    })
                    .flatten()
                    .collect()
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
                let bind_vals = Filter::filter(json, lhs, global_definitions, variable_ctx);
                let orig = json.clone();
                bind_vals
                    .into_iter()
                    .map(|res| match res {
                        Ok(j) => {
                            destructure_pattern(&j, pat, variable_ctx);
                            Ok(orig.clone())
                        }
                        Err(e) => Err(e),
                    })
                    .collect()
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

                let mut acc = acc0;
                for item in gen_items {
                    // bind $var to the generated item
                    variable_ctx.insert(var.clone(), Filter::from_json_const(&item));

                    let upd_results =
                        Filter::filter(&acc, update, global_definitions, variable_ctx);
                    match upd_results.into_iter().find(|r| r.is_ok()) {
                        Some(Ok(next)) => acc = next,
                        Some(Err(e)) => {
                            // restore binding
                            if let Some(prev) = old_binding {
                                variable_ctx.insert(var.clone(), prev);
                            } else {
                                variable_ctx.remove(var);
                            }
                            return vec![Err(e)];
                        }
                        None => {
                            if let Some(prev) = old_binding {
                                variable_ctx.insert(var.clone(), prev);
                            } else {
                                variable_ctx.remove(var);
                            }
                            return vec![Err(JQError::Unknown)];
                        }
                    }
                }

                if let Some(prev) = old_binding {
                    variable_ctx.insert(var.clone(), prev);
                } else {
                    variable_ctx.remove(var);
                }

                vec![Ok(acc)]
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
            Filter::SliceExpression(start, end) => Filter::SliceExpression(
                start.as_ref().map(|s| Box::new(s.substitute(var, arg))),
                end.as_ref().map(|e| Box::new(e.substitute(var, arg))),
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
