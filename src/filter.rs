use std::{
    fmt::{self, Display, Formatter},
    vec,
};

use itertools::Itertools;

use crate::{JQError, Json};

#[derive(Debug, Clone)]
pub enum Filter {
    Dot,                                    // .
    Pipe(Box<Filter>, Box<Filter>),         // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),        // <f_1>, <f_2>
    ObjIndex(String),                       // .<s>
    ArrayIndex(usize),                      // .[<n>]
    ArrayIterator,                          // .[]
    Null,                                   // null
    Boolean(bool),                          // true | false
    Number(f64),                            // 1, 2..
    String(String),                         // "abc"
    Array(Vec<Filter>),                     // [...]
    Object(Vec<(Filter, Filter)>),          // {...}
    BinOp(Box<Filter>, BinOp, Box<Filter>), // <f_1> <op> <f_2>
    Empty,                                  // Empty
    Error(Option<String>),                  // Error, Error("message")
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
    pub fn filter(json: &Json, filter: &Filter) -> Vec<Result<Json, JQError>> {
        match filter {
            Filter::Dot => vec![Ok(json.clone())],
            Filter::Pipe(f1, f2) => {
                let results = Filter::filter(json, f1);
                let results = results
                    .into_iter()
                    .flat_map(|result| result.and_then(|json| Ok(Filter::filter(&json, f2))))
                    .flatten()
                    .collect::<Vec<_>>();
                // let (results, errs): (Vec<_>, Vec<_>) =
                //     results.into_iter().partition(Result::is_ok);
                results
                // if errs.is_empty() {
                //     results
                // } else {
                //     vec![errs[0].clone()]
                // }
            }
            Filter::Comma(f1, f2) => [Filter::filter(json, f1), Filter::filter(json, f2)].concat(),
            Filter::ObjIndex(s) => vec![match json {
                Json::Object(obj) => Ok(obj
                    .iter()
                    .find(|(key, _)| key == s)
                    .map(|(_, value)| value.clone())
                    .unwrap_or(Json::Null)),
                _ => Err(JQError::ObjIndexForNonObject(json.clone(), Json::String(s.clone()))),
            }],
            Filter::ArrayIndex(i) => vec![match json {
                Json::Array(arr) => Ok(arr.get(*i).cloned().unwrap_or(Json::Null)),
                _ => Err(JQError::ArrIndexForNonArray(json.clone(), Json::Number(*i as f64))),
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
                    .map(|f| Filter::filter(json, f))
                    .flatten()
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
                    .map(|(f1, f2)| (Filter::filter(json, f1), Filter::filter(json, f2)))
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
                let ls = Filter::filter(json, l);
                let rs = Filter::filter(json, r);

                return itertools::iproduct!(ls, rs)
                    .map(|(l, r)| match (l, r) {
                        (Ok(Json::Number(l)), Ok(Json::Number(r))) => match bin_op {
                            BinOp::Add => Ok(Json::Number(l + r)),
                            BinOp::Sub => Ok(Json::Number(l - r)),
                            BinOp::Mul => Ok(Json::Number(l * r)),
                            BinOp::Div => Ok(Json::Number(l / r)),
                            BinOp::Mod => Ok(Json::Number(l % r)),
                            BinOp::Eq => Ok(Json::Boolean(l == r)),
                            BinOp::Ne => Ok(Json::Boolean(l != r)),
                            BinOp::Gt => Ok(Json::Boolean(l > r)),
                            BinOp::Ge => Ok(Json::Boolean(l >= r)),
                            BinOp::Lt => Ok(Json::Boolean(l < r)),
                            BinOp::Le => Ok(Json::Boolean(l <= r)),
                            _ => Err(JQError::OpTypeError(
                                Json::Number(l),
                                *bin_op,
                                Json::Number(r),
                            )),
                        },
                        (Ok(Json::Boolean(l)), Ok(Json::Boolean(r))) => match bin_op {
                            BinOp::Eq => Ok(Json::Boolean(l == r)),
                            BinOp::Ne => Ok(Json::Boolean(l != r)),
                            BinOp::And => Ok(Json::Boolean(l && r)),
                            BinOp::Or => Ok(Json::Boolean(l || r)),
                            _ => Err(JQError::OpTypeError(
                                Json::Boolean(l),
                                *bin_op,
                                Json::Boolean(r),
                            )),
                        },
                        (Ok(l), Ok(r)) => Err(JQError::OpTypeError(l, *bin_op, r)),
                        (Err(err), _) => Err(err),
                        (_, Err(err)) => Err(err),
                    })
                    .collect::<Vec<_>>();
            }
            Filter::Empty => todo!(),
            Filter::Error(_) => todo!(),
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::{BinOp, Filter, Json};


    #[test]
    fn test_plus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Add,
            Box::new(Filter::Number(2.0)),
        );

        let results = Filter::filter(&json, &filter);

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

        let results = Filter::filter(&json, &filter);

        assert_eq!(results, vec![Ok(Json::Number(-1.0))]);
    }
}