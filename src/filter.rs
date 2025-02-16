use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    vec,
};

use itertools::Itertools;

use crate::{JQError, Json};

#[derive(Debug, Clone)]
pub enum Filter {
    Dot,                                               // .
    Pipe(Box<Filter>, Box<Filter>),                    // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),                   // <f_1>, <f_2>
    ObjIndex(String),                                  // .<s>
    ArrayIndex(usize),                                 // .[<n>]
    ArrayIterator,                                     // .[]
    Null,                                              // null
    Boolean(bool),                                     // true | false
    Number(f64),                                       // 1, 2..
    String(String),                                    // "abc"
    Array(Vec<Filter>),                                // [...]
    Object(Vec<(Filter, Filter)>),                     // {...}
    BinOp(Box<Filter>, BinOp, Box<Filter>),            // <f_1> <op> <f_2>
    Empty,                                             // Empty
    Error(Option<String>),                             // Error, Error("message")
    Call(String, Option<Vec<Filter>>),                 // <s>(<f_1>, <f_2>...)
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>), // if <f_1> then <f_2> else <f_3>
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
            Filter::BinOp(filter, bin_op, filter1) => {
                write!(f, "{} {} {}", filter, bin_op, filter1)
            }
            Filter::Empty => write!(f, "empty"),
            Filter::Error(None) => write!(f, "error"),
            Filter::Error(Some(s)) => write!(f, "error(\"{}\")", s),
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

impl Filter {
    pub fn filter(
        json: &Json,
        filter: &Filter,
        filters: &HashMap<String, Filter>,
    ) -> Vec<Result<Json, JQError>> {
        match filter {
            Filter::Dot => vec![Ok(json.clone())],
            Filter::Pipe(f1, f2) => Filter::filter(json, f1, filters)
                .into_iter()
                .flat_map(|result| result.map(|json| Filter::filter(&json, f2, filters)))
                .flatten()
                .collect::<Vec<_>>(),
            Filter::Comma(f1, f2) => [
                Filter::filter(json, f1, filters),
                Filter::filter(json, f2, filters),
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
                Json::Array(arr) => Ok(arr.get(*i).cloned().unwrap_or(Json::Null)),
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
                    .flat_map(|f| Filter::filter(json, f, filters))
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
                            Filter::filter(json, f1, filters),
                            Filter::filter(json, f2, filters),
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
            Filter::BinOp(l, bin_op, r) => {
                let ls = Filter::filter(json, l, filters);
                let rs = Filter::filter(json, r, filters);

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
                                (l, r) => Err(JQError::OpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Sub => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l - r)),
                                (Json::Array(l), Json::Array(r)) => Ok(Json::Array(
                                    l.iter().filter(|x| !r.contains(x)).cloned().collect(),
                                )),
                                (l, r) => Err(JQError::OpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mul => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l * r)),
                                (Json::String(s), Json::Number(n))
                                | (Json::Number(n), Json::String(s)) => {
                                    Ok(Json::String(s.repeat(n as usize)))
                                }
                                (Json::Array(l), Json::Number(r)) => Ok(Json::Array(
                                    l.iter().cycle().take(r as usize).cloned().collect(),
                                )),
                                (Json::Number(l), Json::Array(r)) => Ok(Json::Array(
                                    r.iter().cycle().take(l as usize).cloned().collect(),
                                )),
                                (l, r) => Err(JQError::OpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Div => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l / r)),
                                (l, r) => Err(JQError::OpTypeError(l, *bin_op, r)),
                            },
                            BinOp::Mod => match (l, r) {
                                (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l % r)),
                                (l, r) => Err(JQError::OpTypeError(l, *bin_op, r)),
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
            Filter::Empty => todo!(),
            Filter::Error(_) => todo!(),
            Filter::Call(name, filters_) => match filters_ {
                Some(_) => todo!("Call with arguments is not implemented"),
                None => {
                    let filter = filters.get(name).unwrap();
                    Filter::filter(json, filter, filters)
                }
            },
            Filter::IfThenElse(filter, filter1, filter2) => {
                let results = Filter::filter(json, filter, filters);
                results
                    .into_iter()
                    .map(|result| {
                        result.map(|json_| {
                            if let Json::Boolean(true) = json_ {
                                Filter::filter(json, filter1, filters)
                            } else {
                                Filter::filter(json, filter2, filters)
                            }
                        })
                    })
                    .flatten()
                    .flatten()
                    .collect()
            }
        }
    }
}

pub fn builtin_filters() -> HashMap<String, Filter> {
    let id = ("id".to_string(), Filter::Dot);
    let abs = (
        "abs".to_string(),
        Filter::IfThenElse(
            Box::new(Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Lt,
                Box::new(Filter::Number(0.0)),
            )),
            Box::new(Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Mul,
                Box::new(Filter::Number(-1.0)),
            )),
            Box::new(Filter::Dot),
        ),
    );

    let isboolean = (
        "isboolean".to_string(),
        Filter::BinOp(
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
        ),
    );

    let type_ = (
        "type".to_string(),
        Filter::IfThenElse(
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
        ),
    );
    HashMap::from_iter(vec![id, abs, isboolean, type_])
}

#[cfg(test)]
mod tests {
    use std::default;

    use crate::{filter::builtin_filters, BinOp, Filter, Json};

    #[test]
    fn test_plus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Add,
            Box::new(Filter::Number(2.0)),
        );

        let results = Filter::filter(&json, &filter, &Default::default());

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

        let results = Filter::filter(&json, &filter, &Default::default());

        assert_eq!(results, vec![Ok(Json::Number(-1.0))]);
    }

    #[test]
    fn test_abs() {
        let json = Json::Number(-1.0);
        let filter = Filter::Call("abs".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::Number(1.0))]);
    }

    #[test]
    fn test_isboolean1() {
        let json = Json::Boolean(true);
        let filter = Filter::Call("isboolean".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::Boolean(true))]);

        let json = Json::Boolean(false);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::Boolean(true))]);
    }

    #[test]
    fn test_isboolean2() {
        let json = Json::Number(1.0);
        let filter = Filter::Call("isboolean".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::Boolean(false))]);
    }

    #[test]
    fn test_type() {
        let json = Json::Null;
        let filter = Filter::Call("type".to_string(), None);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("null".to_string()))]);

        let json = Json::Boolean(true);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("boolean".to_string()))]);

        let json = Json::Number(1.0);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("number".to_string()))]);

        let json = Json::String("abc".to_string());

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("string".to_string()))]);

        let json = Json::Array(vec![]);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("array".to_string()))]);

        let json = Json::Object(vec![]);

        let results = Filter::filter(&json, &filter, &builtin_filters());

        assert_eq!(results, vec![Ok(Json::String("object".to_string()))]);
    }
}
