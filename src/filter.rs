use std::fmt::{self, Display, Formatter};

use crate::Json;

#[derive(Debug, Clone)]
pub enum Filter {
    Dot,                             // .
    Pipe(Box<Filter>, Box<Filter>),  // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>), // <f_1>, <f_2>
    ObjIndex(String),                // .<s>
    ArrayIndex(usize),               // .[<n>]
    ArrayIterator,                   // .[]
    Null,                            // null
    Boolean(bool),                   // true | false
    Number(f64),                     // 1, 2..
    String(String),                  // "abc"
    Array(Vec<Filter>),              // [...]
    Object(Vec<(Filter, Filter)>),   // {...}
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
        }
    }
}

impl Filter {
    pub fn filter(json: &Json, filter: &Filter) -> Result<Vec<Json>, JQError> {
        match filter {
            Filter::Dot => Ok(vec![json.clone()]),
            Filter::Pipe(f1, f2) => Filter::filter(json, f1).and_then(|j| {
                let (results, errs): (Vec<_>, Vec<_>) = j
                    .iter()
                    .map(|j| Filter::filter(j, f2))
                    .partition(Result::is_ok);

                let results: Vec<Json> =
                    results.into_iter().flat_map(Result::unwrap).collect();
                let errs: Vec<JQError> = errs.into_iter().map(Result::unwrap_err).collect();

                if errs.is_empty() {
                    Ok(results)
                } else {
                    Err(errs.into_iter().next().unwrap())
                }
            }),
            Filter::Comma(f1, f2) => {
                let mut results = Filter::filter(json, f1)?;
                results.extend(Filter::filter(json, f2)?);
                Ok(results)
            }
            Filter::ObjIndex(s) => match json {
                Json::Object(obj) => Ok(obj
                    .iter()
                    .filter(|(key, _)| key == s)
                    .map(|(_, value)| value.clone())
                    .collect()),
                _ => Err(JQError::ObjIndexForNonObject),
            },
            Filter::ArrayIndex(i) => match json {
                Json::Array(arr) => Ok(arr
                    .get(*i).cloned()
                    .into_iter()
                    .collect()),
                _ => Err(JQError::ArrIndexForNonArray),
            },
            Filter::ArrayIterator => match json {
                Json::Array(arr) => Ok(arr.clone()),
                Json::Object(obj) => Ok(obj.iter().map(|(_, value)| value.clone()).collect()),
                _ => Err(JQError::ArrIteratorForNonIterable),
            },
            Filter::Null => Ok(vec![Json::Null]),
            Filter::Boolean(b) => Ok(vec![Json::Boolean(*b)]),
            Filter::Number(n) => Ok(vec![Json::Number(*n)]),
            Filter::String(s) => Ok(vec![Json::String(s.clone())]),
            Filter::Array(arr) => {
                let (results, errs): (Vec<_>, Vec<_>) = arr
                    .iter()
                    .map(|f| Filter::filter(json, f))
                    .partition(Result::is_ok);

                let results: Vec<Vec<Json>> = results.into_iter().map(Result::unwrap).collect();
                let errs: Vec<JQError> = errs.into_iter().map(Result::unwrap_err).collect();

                if errs.is_empty() {
                    Ok(results.iter().map(|v| Json::Array(v.to_vec())).collect())
                } else {
                    Err(errs.into_iter().next().unwrap())
                }
            }
            Filter::Object(obj) => {
                let (results, errs): (Vec<_>, Vec<_>) = obj
                    .iter()
                    .map(|(f1, f2)| (Filter::filter(json, f1), Filter::filter(json, f2)))
                    .partition(|(r1, r2)| r1.is_ok() && r2.is_ok());

                let results: Vec<(Vec<Json>, Vec<Json>)> = results
                    .into_iter()
                    .map(|(r1, r2)| (r1.unwrap(), r2.unwrap()))
                    .collect();

                if errs.is_empty() {
                    let results: Vec<Vec<(Json, Json)>> =
                        results.into_iter().fold(vec![], |mut acc, (keys, values)| {
                            acc.push(itertools::iproduct!(keys, values).collect());
                            acc
                        });

                    let err = results
                        .iter()
                        .flatten()
                        .any(|(k, _)| !matches!(k, Json::String(_)));

                    if err {
                        Err(JQError::NonStringObjectKey)
                    } else {
                        Ok(results
                            .into_iter()
                            .map(|kvs| {
                                kvs.into_iter()
                                    .map(|(k, v)| {
                                        (
                                            if let Json::String(s) = k {
                                                s
                                            } else {
                                                String::new()
                                            },
                                            v,
                                        )
                                    })
                                    .collect()
                            })
                            .map(Json::Object)
                            .collect())
                    }
                } else {
                    // todo: pull the real error out
                    Err(JQError::Unknown)
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum JQError {
    ObjIndexForNonObject,
    ArrIndexForNonArray,
    ArrIteratorForNonIterable,
    NonStringObjectKey,
    Unknown,
}
