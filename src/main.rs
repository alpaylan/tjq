use std::{
    collections::HashMap, fmt::{self, Display, Formatter}, vec
};

use itertools::Itertools;

#[derive(Debug, Clone)]
enum Filter {
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

#[derive(Debug, Clone)]
enum Json {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(Vec<(String, Json)>),
}

impl Display for Json {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Json::Null => write!(f, "null"),
            Json::Boolean(b) => write!(f, "{}", b),
            Json::Number(n) => write!(f, "{}", n),
            Json::String(s) => write!(f, "\"{}\"", s),
            Json::Array(arr) => {
                write!(f, "[")?;
                for (i, j) in arr.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", j)?;
                }
                write!(f, "]")
            }
            Json::Object(obj) => {
                write!(f, "{{")?;
                for (i, (key, value)) in obj.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Constraint {
    Diamond,
    Sum(Box<Constraint>, Box<Constraint>),
    In(Box<Constraint>, Box<Constraint>),
    Field(String),
    Array(Option<usize>),
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Diamond => write!(f, "<>"),
            Constraint::Sum(c1, c2) => write!(f, "({c1} + {c2})"),
            Constraint::In(c1, c2) => write!(f, "{c1}{c2}"),
            Constraint::Field(s) => write!(f, ".{s}"),
            Constraint::Array(Some(u)) => write!(f, ".[{u}]"),
            Constraint::Array(None) => write!(f, ".[]"),
        }
    }
}

impl Constraint {
    pub fn new(f: &Filter) -> Constraint {
        Constraint::build_constraints(f, &Constraint::Diamond)
    }

    pub fn build_constraints(f: &Filter, c: &Constraint) -> Constraint {
        match f {
            Filter::Dot => c.clone(),
            Filter::Pipe(f1, f2) => {
                let x = Constraint::build_constraints(f1, c);
                Constraint::build_constraints(&f2, &x)
            }
            Filter::Comma(f1, f2) => {
                let x = Constraint::build_constraints(f1, c);
                let y = Constraint::build_constraints(f2, c);
                Constraint::Sum(Box::new(x), Box::new(y))
            }
            Filter::Null | Filter::Boolean(_) | Filter::Number(_) | Filter::String(_) => c.clone(),
            Filter::Array(arr) => arr
                .iter()
                .map(|j| Constraint::build_constraints(j, c))
                .fold(Constraint::Diamond, |acc, x| {
                    Constraint::Sum(Box::new(acc), Box::new(x))
                }),
            Filter::Object(obj) => obj
                .iter()
                .map(|(j1, j2)| {
                    Constraint::Sum(
                        Box::new(Constraint::build_constraints(j1, c)),
                        Box::new(Constraint::build_constraints(j2, c)),
                    )
                })
                .fold(Constraint::Diamond, |acc, x| {
                    Constraint::Sum(Box::new(acc), Box::new(x))
                }),
            Filter::ObjIndex(s) => {
                Constraint::In(Box::new(c.clone()), Box::new(Constraint::Field(s.clone())))
            }
            Filter::ArrayIndex(n) => {
                Constraint::In(Box::new(c.clone()), Box::new(Constraint::Array(Some(*n))))
            }
            Filter::ArrayIterator => {
                Constraint::In(Box::new(c.clone()), Box::new(Constraint::Array(None)))
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Shape {
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
enum Access {
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
struct ShapeMismatch {
    path: Vec<Access>,
    expected: Shape,
    got: Json,
}

impl Display for ShapeMismatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Shape mistmatch detected!\n");

        write!(f, "\tat ");
        for path in self.path.iter() {
            write!(f, "{path}");
        }
        write!(f, "\n");

        write!(f, "\tExpected: {}\n", self.expected);
        write!(f, "\tGot: {}\n", self.got)
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
            Constraint::Array(Some(u)) => todo!(),
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
                        while u as usize >= tuple.len() {
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
                        .into_iter()
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
                    let obj_map = obj.clone().into_iter().map(|(s, j)| (s, j)).collect::<HashMap<String, Json>>();

                    let (_, mismatches) : (Vec<_>, Vec<_>) = obj_shape.into_iter().map(|(key,shape)| {
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

#[derive(Debug)]
enum JQError {
    ObjIndexForNonObject,
    ArrIndexForNonArray,
    ArrIteratorForNonIterable,
    NonStringObjectKey,
    Unknown,
}

impl Json {
    fn filter(&self, filter: &Filter) -> Result<Vec<Json>, JQError> {
        match filter {
            Filter::Dot => Ok(vec![self.clone()]),
            Filter::Pipe(f1, f2) => self.filter(f1).and_then(|j| {
                let (results, errs): (Vec<_>, Vec<_>) =
                    j.iter().map(|j| j.filter(f2)).partition(Result::is_ok);

                let results: Vec<Json> =
                    results.into_iter().map(Result::unwrap).flatten().collect();
                let errs: Vec<JQError> = errs.into_iter().map(Result::unwrap_err).collect();

                if errs.is_empty() {
                    Ok(results)
                } else {
                    Err(errs.into_iter().next().unwrap())
                }
            }),
            Filter::Comma(f1, f2) => {
                let mut results = self.filter(f1)?;
                results.extend(self.filter(f2)?);
                Ok(results)
            }
            Filter::ObjIndex(s) => match self {
                Json::Object(obj) => Ok(obj
                    .iter()
                    .filter(|(key, _)| key == s)
                    .map(|(_, value)| value.clone())
                    .collect()),
                _ => Err(JQError::ObjIndexForNonObject),
            },
            Filter::ArrayIndex(i) => match self {
                Json::Array(arr) => Ok(arr
                    .get(*i as usize)
                    .map(|j| j.clone())
                    .into_iter()
                    .collect()),
                _ => Err(JQError::ArrIndexForNonArray),
            },
            Filter::ArrayIterator => match self {
                Json::Array(arr) => Ok(arr.clone()),
                Json::Object(obj) => Ok(obj.iter().map(|(_, value)| value.clone()).collect()),
                _ => Err(JQError::ArrIteratorForNonIterable),
            },
            Filter::Null => Ok(vec![Json::Null]),
            Filter::Boolean(b) => Ok(vec![Json::Boolean(*b)]),
            Filter::Number(n) => Ok(vec![Json::Number(*n)]),
            Filter::String(s) => Ok(vec![Json::String(s.clone())]),
            Filter::Array(arr) => {
                let (results, errs): (Vec<_>, Vec<_>) =
                    arr.iter().map(|f| self.filter(f)).partition(Result::is_ok);

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
                    .map(|(f1, f2)| (self.filter(f1), self.filter(f2)))
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

                    let err = results.iter().flatten().any(|(k, _)| {
                        if let Json::String(_) = k {
                            false
                        } else {
                            true
                        }
                    });

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
                            .map(|kvs| Json::Object(kvs))
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

fn main() {
    let filter = Filter::Pipe(
        Box::new(Filter::Pipe(
            Box::new(Filter::ArrayIterator),
            Box::new(Filter::Comma(
                Box::new(Filter::ObjIndex("age".to_string())),
                Box::new(Filter::ObjIndex("name".to_string())),
            )),
        )),
        Box::new(Filter::Object(vec![(
            Filter::String("v".to_string()),
            Filter::ObjIndex("a".to_string()),
        )])),
    );

    let json = Json::Array(vec![
        Json::Object(vec![
            ("name".to_string(), Json::String("John".to_string())),
            ("age".to_string(), Json::Number(25.0)),
        ]),
        Json::Object(vec![
            ("name".to_string(), Json::String("Jane".to_string())),
            ("age".to_string(), Json::Number(30.0)),
        ]),
    ]);

    println!("Input: {}", json);
    println!("Filter: {}", filter);

    let result = json.filter(&filter);

    match result {
        Ok(results) => {
            if results.is_empty() {
                println!("null");
            }

            for result in results {
                println!("{}", result);
            }
        }
        Err(err) => {
            println!("error: {:?}", err);
        }
    }

    let c = Constraint::new(&filter);

    println!("Constraint: {}", c);

    let s = Shape::new(c);

    match s {
        Ok(s) => {
            println!("Shape: {}", s);

            let m = s.check(json, vec![]);

            match m {
                Some(m) => println!("{m}"),
                None => println!("The input conforms to the inferred shape"),
            }
        }
        Err(err) => {
            println!("error: {:?}", err);
        }
    }
}
