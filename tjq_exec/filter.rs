use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    vec,
};

use itertools::Itertools;

use crate::error::JQError;
use crate::json::Json;

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    Dot,                                                      // .
    Pipe(Box<Filter>, Box<Filter>),                           // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),                          // <f_1>, <f_2>
    ObjIndex(String),                                         // .<s>
    ArrayIndex(isize),                                        // .[<n>]
    ArrayIterator,                                            // .[]
    Null,                                                     // null
    Boolean(bool),                                            // true | false
    Number(f64),                                              // 1, 2..
    String(String),                                           // "abc"
    Array(Vec<Filter>),                                       // [...]
    Object(Vec<(Filter, Filter)>),                            // {...}
    UnOp(UnOp, Box<Filter>),                                  // <op> <f>
    BinOp(Box<Filter>, BinOp, Box<Filter>),                   // <f_1> <op> <f_2>
    Empty,                                                    // Empty
    Error,                                                    // Error
    Call(String, Option<Vec<Filter>>),                        // <s>(<f_1>, <f_2>...)
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>),        // if <f_1> then <f_2> else <f_3>
    Bound(Vec<String>, Box<Filter>),                          // \<s_1>, <s_2>... <f>
    FunctionExpression(HashMap<String, Filter>, Box<Filter>), // local_defs, <f>
    BindingExpression(Box<Filter>, Box<Filter>),              //
    Variable(String),                                         // $var
    ReduceExpression(HashMap<String, Filter>, Box<Filter>, Box<Filter>),
    // SliceExpression(i32,i32),                                        // [<n1> : <n2>]
    Hole, // Placeholder for a missing value in the AST
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
            Filter::ReduceExpression(var_def, expr, num) => {
                for (name, local_filter) in var_def {
                    write!(f, "variable")?;
                }
                write!(f, "{}", expr)
            }
            Filter::Hole => write!(f, "(_)"),
            // Filter::SliceExpression(i32:, )
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

impl Filter {
    pub fn filter(
        json: &Json,
        filter: &Filter,
        global_definitions: &HashMap<String, Filter>,
        variable_ctx: &mut HashMap<String, Filter>,
    ) -> Vec<Result<Json, JQError>> {
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
            Filter::ObjIndex(s) => vec![match json {
                Json::Object(obj) => Ok(obj
                    .iter()
                    .find(|(key, _)| key == s)
                    .map(|(_, value)| value.clone())
                    .unwrap_or(Json::Null)),
                _ => Err(JQError::ObjIndexForNonObject(
                    json.clone(),
                    Json::String(s.clone()),
                )),
            }],
            Filter::ArrayIndex(i) => vec![match json {
                Json::Array(arr) => {
                    let i = if *i < 0 { arr.len() as isize + i } else { *i } as usize;

                    Ok(arr.get(i).cloned().unwrap_or(Json::Null))
                }
                _ => Err(JQError::ArrIndexForNonArray(
                    json.clone(),
                    Json::Number(*i as f64),
                )),
            }],
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
                                    obj.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
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
            Filter::BinOp(l, bin_op, r) => {
                let ls = Filter::filter(json, l, global_definitions, variable_ctx);
                let rs = Filter::filter(json, r, global_definitions, variable_ctx);

                itertools::iproduct!(ls, rs)
                    .map(|(l, r)| match (l, r) {
                        (Err(err), _) | (_, Err(err)) => Err(err),
                        (Ok(l), Ok(r)) => match bin_op {
                            BinOp::Add => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l + r)),
                                (Json::String(l), Json::String(r)) => {
                                    Ok(Json::String(format!("{}{}", l, r)))
                                }
                                (Json::Array(l), Json::Array(r)) => {
                                    Ok(Json::Array([l, r].concat()))
                                }
                                (Json::Object(l), Json::Object(r)) => {
                                    Ok(Json::Object([l, r].concat()))
                                }
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Sub => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l - r)),
                                (Json::Array(l), Json::Array(r)) => Ok(Json::Array(
                                    l.iter().filter(|x| !r.contains(x)).cloned().collect(),
                                )),
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mul => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l * r)),
                                (Json::String(s), Json::Number(n))
                                | (Json::Number(n), Json::String(s)) => {
                                    if n < 0.0 {
                                        Ok(Json::Null)
                                    } else {
                                        Ok(Json::String(s.repeat(n as usize)))
                                    }
                                }
                                (Json::Array(l), Json::Number(r)) => Ok(Json::Array(
                                    l.iter().cycle().take(r as usize).cloned().collect(),
                                )),
                                (Json::Number(l), Json::Array(r)) => Ok(Json::Array(
                                    r.iter().cycle().take(l as usize).cloned().collect(),
                                )),
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Div => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l / r)),
                                (l, r) => Err(JQError::BinOpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mod => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l % r)),
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
                    // Find the filter with the given name
                    let filter = global_definitions
                        .get(name)
                        .unwrap_or_else(|| panic!("Filter '{name}' not found"));
                    // The filter should have the same number of arguments as the number of arguments passed
                    if let Filter::Bound(params, filter) = filter {
                        if params.len() != args.len() {
                            return vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))];
                        }
                        // Bind the arguments to the parameters
                        let mut filters = global_definitions.clone();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            filters.insert(param.clone(), arg.clone());
                        }
                        Filter::filter(json, filter, &filters, variable_ctx)
                    } else {
                        vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))]
                    }
                }
                None => {
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
                            if let Json::Boolean(true) = json_ {
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
            Filter::ReduceExpression(var_def, init, update) => {
                let init_results = Filter::filter(json, init, global_definitions, variable_ctx);
                let acc = match &init_results[0] {
                    Ok(Json::Number(n)) => *n,
                    _ => panic!(
                        "reduce init must produce a number(other types are not implemented yet)"
                    ),
                };
                let mut acc = acc;

                for (var_name, src_filter) in var_def {
                    let elems = Filter::filter(json, src_filter, global_definitions, variable_ctx);
                    for e in elems {
                        let elem_json = e.clone().unwrap();

                        let lit = match elem_json {
                            Json::Number(n) => Filter::Number(n),
                            Json::Boolean(b) => Filter::Boolean(b),
                            Json::String(s) => Filter::String(s),
                            Json::Null => Filter::Null,
                            _ => panic!("reduce only supports primitives for now"),
                        };
                        variable_ctx.insert(var_name.clone(), lit);

                        let upd_results = Filter::filter(
                            &Json::Number(acc),
                            update,
                            global_definitions,
                            variable_ctx,
                        );
                        acc = match &upd_results[0] {
                            Ok(Json::Number(n)) => *n,
                            _ => panic!("reduce update must produce a number"),
                        };
                    }
                }

                vec![Ok(Json::Number(acc))]
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
                    Filter::Bound(
                        items.clone(),
                        Box::new(filter.substitute(var, arg)),
                    )
                }
            },
            Filter::BindingExpression(_, _) => todo!(),
            Filter::Variable(_) => todo!(),
            Filter::ReduceExpression(_, _, _) => todo!(),
            Filter::Hole => todo!(),
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

    use crate::{BinOp, Filter, Json, UnOp};

    fn builtin_filters() -> HashMap<String, Filter> {
        let map = Filter::Bound(
            vec!["f".into()],
            Box::new(Filter::Array(vec![Filter::Pipe(
                Box::new(Filter::ArrayIterator),
                Box::new(Filter::Call("f".to_string(), None)),
            )])),
        );

        let abs = Filter::Bound(
            vec![],
            Box::new(Filter::IfThenElse(
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Lt,
                    Box::new(Filter::Number(0.0)),
                )),
                Box::new(Filter::UnOp(UnOp::Neg, Box::new(Filter::Dot))),
                Box::new(Filter::Dot),
            )),
        );

        let isboolean = Filter::Bound(
            vec![],
            Box::new(Filter::BinOp(
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Eq,
                    Box::new(Filter::Boolean(true)),
                )),
                BinOp::Or,
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Eq,
                    Box::new(Filter::Boolean(false)),
                )),
            )),
        );

        // def type:
        //     if . == null then "null"
        //     elif isboolean then "boolean"
        //     elif . < "" then "number"
        //     elif . < [] then "string"
        //     elif . < {} then "array"
        //     else             "object" end;
        let type_ = Filter::Bound(
            vec![],
            Box::new(Filter::IfThenElse(
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Eq,
                    Box::new(Filter::Null),
                )),
                Box::new(Filter::String("null".to_string())),
                Box::new(Filter::IfThenElse(
                    Box::new(Filter::Call("isboolean".to_string(), None)),
                    Box::new(Filter::String("boolean".to_string())),
                    Box::new(Filter::IfThenElse(
                        Box::new(Filter::BinOp(
                            Box::new(Filter::Dot),
                            BinOp::Lt,
                            Box::new(Filter::String("".to_string())),
                        )),
                        Box::new(Filter::String("number".to_string())),
                        Box::new(Filter::IfThenElse(
                            Box::new(Filter::BinOp(
                                Box::new(Filter::Dot),
                                BinOp::Lt,
                                Box::new(Filter::Array(vec![])),
                            )),
                            Box::new(Filter::String("string".to_string())),
                            Box::new(Filter::IfThenElse(
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Dot),
                                    BinOp::Lt,
                                    Box::new(Filter::Object(vec![])),
                                )),
                                Box::new(Filter::String("array".to_string())),
                                Box::new(Filter::String("object".to_string())),
                            )),
                        )),
                    )),
                )),
            )),
        );
        let while_ = Filter::Bound(
            vec!["f".into()],
            Box::new(Filter::IfThenElse(
                Box::new(Filter::Call("f".to_string(), None)),
                Box::new(Filter::Pipe(
                    Box::new(Filter::ArrayIterator),
                    Box::new(Filter::Call("f".to_string(), None)),
                )),
                Box::new(Filter::Empty),
            )),
        );

        let mut filters = HashMap::new();
        filters.insert("map".to_string(), map);
        filters.insert("abs".to_string(), abs);
        filters.insert("isboolean".to_string(), isboolean);
        filters.insert("type".to_string(), type_);
        filters.insert("while".to_string(), while_);

        filters
    }

    #[test]
    fn test_plus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Add,
            Box::new(Filter::Number(2.0)),
        );

        let results = Filter::filter(&json, &filter, &Default::default(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Number(3.0))]);
    }

    #[test]
    fn test_minus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Sub,
            Box::new(Filter::Number(2.0)),
        );

        let results = Filter::filter(&json, &filter, &Default::default(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Number(-1.0))]);
    }

    #[test]
    fn test_abs() {
        let json = Json::Number(-1.0);
        let filter = Filter::Call("abs".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Number(1.0))]);
    }

    #[test]
    fn test_isboolean1() {
        let json = Json::Boolean(true);
        let filter = Filter::Call("isboolean".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Boolean(true))]);

        let json = Json::Boolean(false);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Boolean(true))]);
    }

    #[test]
    fn test_isboolean2() {
        let json = Json::Number(1.0);
        let filter = Filter::Call("isboolean".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::Boolean(false))]);
    }

    #[test]
    fn test_type() {
        let json = Json::Null;
        let filter = Filter::Call("type".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("null".to_string()))]);

        let json = Json::Boolean(true);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("boolean".to_string()))]);

        let json = Json::Number(1.0);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("number".to_string()))]);

        let json = Json::String("abc".to_string());

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("string".to_string()))]);

        let json = Json::Array(vec![]);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("array".to_string()))]);

        let json = Json::Object(vec![]);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(results, vec![Ok(Json::String("object".to_string()))]);
    }

    #[test]
    fn test_map() {
        let json = Json::Array(vec![Json::Number(-1.0), Json::Number(2.0)]);
        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::Call("abs".to_string(), None)]),
        );

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(
            results,
            vec![Ok(Json::Array(vec![Json::Number(1.0), Json::Number(2.0)]))]
        );
    }

    #[test]
    fn test_map2() {
        let json = Json::Array(vec![Json::Number(-1.0), Json::Number(2.0)]);
        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::Bound(
                vec![],
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Mul,
                    Box::new(Filter::Number(2.0)),
                )),
            )]),
        );

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(
            results,
            vec![Ok(Json::Array(vec![Json::Number(-2.0), Json::Number(4.0)]))]
        );
    }

    #[test]
    fn test_map3() {
        let json = Json::Array(vec![
            Json::Array(vec![Json::Number(-1.0), Json::Number(-2.0)]),
            Json::Array(vec![Json::Number(3.0), Json::Number(4.0)]),
        ]);
        println!("Input: {}", json);
        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::Call(
                "map".to_string(),
                Some(vec![Filter::Call("abs".to_string(), None)]),
            )]),
        );
        println!("Filter: {}", filter);

        let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

        assert_eq!(
            results,
            vec![Ok(Json::Array(vec![
                Json::Array(vec![Json::Number(1.0), Json::Number(2.0)]),
                Json::Array(vec![Json::Number(3.0), Json::Number(4.0)]),
            ]))]
        );

        for result in results {
            println!("Result: {}", result.unwrap());
        }
    }

    #[test]
    fn test_interpret_function_definition() {
        let filter = EnvFilter::from_default_env();
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_file(true)
            .with_line_number(true)
            .init();

        let mut defs = HashMap::new();
        defs.insert(
            "main".to_string(),
            Filter::Bound(
                vec![],
                Box::new(Filter::FunctionExpression(
                    HashMap::from([
                        (
                            "add".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    BinOp::Add,
                                    Box::new(Filter::Call("b".to_string(), None)),
                                )),
                            ),
                        ),
                        (
                            "sub".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    BinOp::Sub,
                                    Box::new(Filter::Call("b".to_string(), None)),
                                )),
                            ),
                        ),
                        (
                            "mul".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    BinOp::Mul,
                                    Box::new(Filter::Call("b".to_string(), None)),
                                )),
                            ),
                        ),
                    ]),
                    Box::new(Filter::Call(
                        "add".to_string(),
                        Some(vec![Filter::Number(1.0), Filter::Number(2.0)]),
                    )),
                )),
            ),
        );

        let filter = Filter::Call("main".to_string(), None);
        let results = Filter::filter(&Json::Null, &filter, &defs, &mut Default::default());
        assert_eq!(
            results,
            vec![Ok(Json::Number(3.0))],
            "Expected 3.0, got: {:?}",
            results
        );
    }
}
