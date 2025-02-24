use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    vec,
};

use itertools::Itertools;

use crate::{parse_defs, JQError, Json};

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    Dot,                                               // .
    Pipe(Box<Filter>, Box<Filter>),                    // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),                   // <f_1>, <f_2>
    ObjIndex(String),                                  // .<s>
    ArrayIndex(isize),                                 // .[<n>]
    ArrayIterator,                                     // .[]
    Null,                                              // null
    Boolean(bool),                                     // true | false
    Number(f64),                                       // 1, 2..
    String(String),                                    // "abc"
    Array(Vec<Filter>),                                // [...]
    Object(Vec<(Filter, Filter)>),                     // {...}
    UnOp(UnOp, Box<Filter>),                           // <op> <f>
    BinOp(Box<Filter>, BinOp, Box<Filter>),            // <f_1> <op> <f_2>
    Empty,                                             // Empty
    Error(Option<String>),                             // Error, Error("message")
    Call(String, Option<Vec<Filter>>),                 // <s>(<f_1>, <f_2>...)
    IfThenElse(Box<Filter>, Box<Filter>, Box<Filter>), // if <f_1> then <f_2> else <f_3>
    Bound(Vec<String>, Box<Filter>),                   // \<s_1>, <s_2>... <f>
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
    Not, // not
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
            Filter::Bound(_, filter) => {
                write!(f, " {}", filter)
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

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "not"),
        }
    }
}

impl Filter {
    pub fn filter(
        json: &Json,
        filter: &Filter,
        filters: &HashMap<String, Filter>,
        variable_ctx: &mut HashMap<String, Filter>,
    ) -> Vec<Result<Json, JQError>> {
        match filter {
            Filter::Dot => vec![Ok(json.clone())],
            Filter::Pipe(f1, f2) => Filter::filter(json, f1, filters, variable_ctx)
                .into_iter()
                .flat_map(|result| {
                    result.map(|json| Filter::filter(&json, f2, filters, variable_ctx))
                })
                .flatten()
                .collect::<Vec<_>>(),
            Filter::Comma(f1, f2) => [
                Filter::filter(json, f1, filters, variable_ctx),
                Filter::filter(json, f2, filters, variable_ctx),
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
                    .flat_map(|f| Filter::filter(json, f, filters, variable_ctx))
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
                            Filter::filter(json, f1, filters, variable_ctx),
                            Filter::filter(json, f2, filters, variable_ctx),
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
                let results = Filter::filter(json, f, filters, variable_ctx);
                results
                    .into_iter()
                    .map(|result| match result {
                        Ok(json) => match un_op {
                            UnOp::Neg => match json {
                                Json::Number(n) => Ok(Json::Number(-n)),
                                _ => Err(JQError::UnOpTypeError(json, *un_op)),
                            },
                            UnOp::Not => match json {
                                Json::Boolean(b) => Ok(Json::Boolean(!b)),
                                _ => Err(JQError::UnOpTypeError(json, *un_op)),
                            },
                        },
                        Err(err) => Err(err),
                    })
                    .collect()
            }
            Filter::BinOp(l, bin_op, r) => {
                let ls = Filter::filter(json, l, filters, variable_ctx);
                let rs = Filter::filter(json, r, filters, variable_ctx);

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
            Filter::Error(_) => todo!(),
            Filter::Call(name, filters_) => match filters_ {
                Some(args) => {
                    // Find the filter with the given name
                    let filter = filters
                        .get(name)
                        .expect(format!("Filter '{name}' not found").as_str());
                    // The filter should have the same number of arguments as the number of arguments passed
                    if let Filter::Bound(params, filter) = filter {
                        if params.len() != args.len() {
                            return vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))];
                        }
                        // Bind the arguments to the parameters
                        let mut filters = filters.clone();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            filters.insert(param.clone(), arg.clone());
                        }
                        Filter::filter(json, filter, &filters, variable_ctx)
                    } else {
                        vec![Err(JQError::FilterNotDefined(name.clone(), args.len()))]
                    }
                }
                None => {
                    let filter = filters
                        .get(name)
                        .expect(format!("Filter '{name}' not found").as_str());
                    Filter::filter(json, filter, filters, variable_ctx)
                }
            },
            Filter::IfThenElse(filter, filter1, filter2) => {
                let results = Filter::filter(json, filter, filters, variable_ctx);
                results
                    .into_iter()
                    .map(|result| {
                        result.map(|json_| {
                            if let Json::Boolean(true) = json_ {
                                Filter::filter(json, filter1, filters, variable_ctx)
                            } else {
                                Filter::filter(json, filter2, filters, variable_ctx)
                            }
                        })
                    })
                    .flatten()
                    .flatten()
                    .collect()
            }
            Filter::Bound(items, filter) => {
                for item in items {
                    todo!()
                }

                Filter::filter(json, filter, filters, variable_ctx)
            }
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
            | Filter::Error(_) => self.clone(),
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
            Filter::IfThenElse(filter, filter1, filter2) => Filter::IfThenElse(
                Box::new(filter.substitute(var, arg)),
                Box::new(filter1.substitute(var, arg)),
                Box::new(filter2.substitute(var, arg)),
            ),
            Filter::Bound(items, filter) => todo!(),
        }
    }
}

pub fn builtin_filters() -> HashMap<String, Filter> {
    // read defs.jq
    let defs = parse_defs(include_str!("defs.jq"));
    defs
}

#[cfg(test)]
mod tests {

    use std::vec;

    use crate::{filter::builtin_filters, BinOp, Filter, Json};

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
}
