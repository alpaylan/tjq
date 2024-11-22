use std::{collections::HashMap, fmt::{self, Display, Formatter}};

use itertools::Itertools;

use crate::{Constraint, JQError, Json};

#[derive(Debug, Clone)]
pub enum Shape {
    Blob,
    Null,
    Bool,
    Number,
    String,
    Array(Box<Shape>, Option<usize>),
    Tuple(Vec<Shape>),
    Object(Vec<(String, Shape)>),
}

#[derive(Debug, Clone)]
pub enum Access {
    Field(String),
    Array(usize),
}

impl Display for Access {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Access::Field(s) => write!(f, ".{s}"),
            Access::Array(u) => write!(f, "[{u}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ShapeMismatch {
    path: Vec<Access>,
    expected: Shape,
    got: Json,
}

impl Display for ShapeMismatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Shape mistmatch detected!")?;

        write!(f, "\tat ")?;
        for path in self.path.iter() {
            write!(f, "{path}")?;
        }
        writeln!(f)?;

        writeln!(f, "\tExpected: {}", self.expected)?;
        writeln!(f, "\tGot: {}", self.got)
    }
}

impl ShapeMismatch {
    pub fn new(path: Vec<Access>, expected: Shape, got: Json) -> Self {
        ShapeMismatch {
            path,
            expected,
            got,
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Shape::Blob => write!(f, "<>"),
            Shape::Null => write!(f, "<null>"),
            Shape::Bool => write!(f, "<bool>"),
            Shape::Number => write!(f, "<number>"),
            Shape::String => write!(f, "<string>"),
            Shape::Array(shape, Some(n)) => write!(f, "[{shape} ; {n}]"),
            Shape::Array(shape, None) => write!(f, "[{shape}]"),
            Shape::Tuple(tuple) => {
                write!(f, "[")?;
                for (i, j) in tuple.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", j)?;
                }
                write!(f, "]")
            }
            Shape::Object(obj) => {
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

impl Shape {
    pub fn new(c: Constraint) -> Result<Shape, JQError> {
        Shape::build_shape(c, Shape::Blob)
    }

    /// Build shape with continuation:
    ///     - When `Constraint::In` is used, the second part of the constraint must be calculated
    ///     at the leaves the first one produces.
    pub fn build_shape2(c1: Constraint, c2: Constraint, s: Shape) -> Result<Shape, JQError> {
        match c1 {
            Constraint::Diamond => Shape::build_shape(c2, s),
            Constraint::Sum(c11, c12) => {
                let s1 = Shape::build_shape2(*c11, c2.clone(), s)?;
                Shape::build_shape2(*c12, c2, s1)
            }
            Constraint::In(c11, c12) => {
                Shape::build_shape2(*c11, Constraint::In(c12, Box::new(c2)), s)
            }
            Constraint::Field(f) => match s {
                Shape::Blob => Ok(Shape::Object(vec![(
                    f,
                    Shape::build_shape(c2, Shape::Blob)?,
                )])),
                Shape::Null => Ok(Shape::Null),
                Shape::Bool
                | Shape::Number
                | Shape::String
                | Shape::Array(_, _)
                | Shape::Tuple(_) => Err(JQError::ObjIndexForNonObject),
                Shape::Object(mut obj) => {
                    let inner = obj.clone().into_iter().find_position(|(k, _)| *k == f);

                    if let Some((i, (key, shape))) = inner {
                        obj.remove(i);

                        obj.push((key.to_string(), Shape::build_shape(c2, shape.clone())?));

                    } else {
                        obj.push((f, Shape::build_shape(c2, Shape::Blob)?));
                        
                    }

                    Ok(Shape::Object(obj))
                }
            },
            Constraint::Array(None) => match s {
                Shape::Blob => Ok(Shape::Array(
                    Box::new(Shape::build_shape(c2, Shape::Blob)?),
                    None,
                )),
                Shape::Null | Shape::Bool | Shape::Number | Shape::String => {
                    Err(JQError::ArrIteratorForNonIterable)
                }
                Shape::Array(shape, u) => {
                    Ok(Shape::Array(Box::new(Shape::build_shape(c2, *shape)?), u))
                }
                Shape::Tuple(tuple) => {
                    let (results, errs): (Vec<_>, Vec<_>) = tuple
                        .into_iter()
                        .map(|s| Shape::build_shape(c2.clone(), s))
                        .partition(Result::is_ok);

                    let results: Vec<Shape> = results.into_iter().map(Result::unwrap).collect();
                    let errs: Vec<JQError> = errs.into_iter().map(Result::unwrap_err).collect();

                    if errs.is_empty() {
                        Ok(Shape::Tuple(results))
                    } else {
                        Err(errs.into_iter().next().unwrap())
                    }
                }
                Shape::Object(obj) => {
                    let (results, errs): (Vec<_>, Vec<_>) = obj
                        .into_iter()
                        .map(|(k, s)| (k, Shape::build_shape(c2.clone(), s)))
                        .partition(|(_, r)| r.is_ok());

                    let results: Vec<(String, Shape)> =
                        results.into_iter().map(|(s, r)| (s, r.unwrap())).collect();
                    let errs: Vec<JQError> =
                        errs.into_iter().map(|(_, r)| r.unwrap_err()).collect();

                    if errs.is_empty() {
                        Ok(Shape::Object(results))
                    } else {
                        Err(errs.into_iter().next().unwrap())
                    }
                }
            },
            Constraint::Array(Some(_)) => todo!(),
        }
    }

    pub fn build_shape(c: Constraint, s: Shape) -> Result<Shape, JQError> {
        match c {
            Constraint::Diamond => Ok(s),
            Constraint::Sum(c1, c2) => {
                let s1 = Shape::build_shape(*c1, s)?;
                Shape::build_shape(*c2, s1)
            }
            Constraint::In(c1, c2) => Shape::build_shape2(*c1, *c2, s),
            Constraint::Field(f) => match s {
                Shape::Blob => Ok(Shape::Object(vec![(f.to_string(), Shape::Blob)])),
                Shape::Null => Ok(Shape::Null),
                Shape::Bool => Err(JQError::ObjIndexForNonObject),
                Shape::Number => Err(JQError::ObjIndexForNonObject),
                Shape::String => Err(JQError::ObjIndexForNonObject),
                Shape::Array(_, _) => Err(JQError::ObjIndexForNonObject),
                Shape::Tuple(_) => Err(JQError::ObjIndexForNonObject),
                Shape::Object(mut obj) => {
                    let has_key = obj.iter().any(|(k, _)| *k == f);

                    if !has_key {
                        obj.push((f, Shape::Blob));
                    }

                    Ok(Shape::Object(obj))
                }
            },
            Constraint::Array(u) => match s {
                Shape::Blob => Ok(Shape::Array(Box::new(Shape::Blob), u)),
                Shape::Null => {
                    if u.is_some() {
                        Ok(Shape::Array(Box::new(Shape::Blob), u))
                    } else {
                        Err(JQError::ArrIteratorForNonIterable)
                    }
                }
                // todo: make these errors concrete
                Shape::Bool => Err(JQError::Unknown),
                Shape::Number => Err(JQError::Unknown),
                Shape::String => Err(JQError::Unknown),
                Shape::Array(shape, n) => match (u, n) {
                    (None, None) => Ok(Shape::Array(shape, None)),
                    (None, Some(n)) => Ok(Shape::Array(shape, Some(n))),
                    (Some(u), None) => Ok(Shape::Array(shape, Some(u))),
                    (Some(u), Some(n)) => Ok(Shape::Array(shape, Some(u.max(n)))),
                },
                Shape::Tuple(mut tuple) => match u {
                    Some(u) => {
                        while u >= tuple.len() {
                            tuple.push(Shape::Blob);
                        }

                        Ok(Shape::Tuple(tuple))
                    }
                    None => Ok(Shape::Tuple(tuple)),
                },
                Shape::Object(_) => Err(JQError::ArrIndexForNonArray),
            },
        }
    }

    pub fn check(&self, j: Json, path: Vec<Access>) -> Option<ShapeMismatch> {
        match self {
            Shape::Blob => None,
            Shape::Null => {
                if let Json::Null = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            }
            Shape::Bool => {
                if let Json::Boolean(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            }
            Shape::Number => {
                if let Json::Number(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            }
            Shape::String => {
                if let Json::String(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            }
            Shape::Array(shape, u) => {
                if let Json::Array(arr) = j {

                    if let Some(u) = u {
                        if *u > arr.len() {
                            return Some(ShapeMismatch::new(path, self.clone(), Json::Array(arr)));
                        }
                    }

                    let (_, mismatches) : (Vec<_>, Vec<_>) = arr
                        .into_iter()
                        .enumerate()
                        .map(|(i, j)| {
                            Shape::check(shape, j, [path.clone(), vec![Access::Array(i)]].concat())
                        })
                        .partition(Option::is_none);
                    
                    if mismatches.is_empty() {
                        None
                    } else {
                        mismatches.into_iter().next().unwrap()
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            }
            Shape::Tuple(tuple) => {
                if let Json::Array(arr) = j {
                    if tuple.len() > arr.len() {
                        return Some(ShapeMismatch::new(path, self.clone(), Json::Array(arr)));
                    }

                    let (_, mismatches) : (Vec<_>, Vec<_>) = tuple
                        .iter()
                        .enumerate()
                        .zip(arr)
                        .map(|((i, s), j)| {
                            Shape::check(s, j, [path.clone(), vec![Access::Array(i)]].concat())
                        })
                        .partition(Option::is_none);
                    
                    if mismatches.is_empty() {
                        None
                    } else {
                        mismatches.into_iter().next().unwrap()
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            },
            Shape::Object(obj_shape) => {
                if let Json::Object(obj) = j {
                    let obj_map = obj.clone().into_iter().collect::<HashMap<String, Json>>();

                    let (_, mismatches) : (Vec<_>, Vec<_>) = obj_shape.iter().map(|(key,shape)| {
                        let v = obj_map.get(key);

                        if let Some(v) = v {
                            Shape::check(shape, v.clone(), [path.clone(), vec![Access::Field(key.to_string())]].concat())
                        } else {
                            Some(ShapeMismatch::new(path.clone(), self.clone(), Json::Object(obj.clone())))
                        }
                    }).partition(Option::is_none);

                    if mismatches.is_empty() {
                        None
                    } else {
                        mismatches.into_iter().next().unwrap()
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), j))
                }
            },
        }
    }
}