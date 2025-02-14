use std::{
    clone,
    collections::HashMap,
    fmt::{self, format, Display, Formatter}, hash::Hash,
};

use itertools::Itertools;
use topological_sort::TopologicalSort;

use crate::{Filter, Json};

#[derive(Debug, Clone, PartialEq)]
pub enum Shape {
    TVar(String),
    Blob,
    Null,
    Bool(Option<bool>),
    Number(Option<f64>),
    String(Option<String>),
    Array(Box<Shape>, Option<usize>),
    Tuple(Vec<Shape>),
    Object(Vec<(String, Shape)>),
    Mismatch(Box<Shape>, Box<Shape>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Access {
    Field(String),
    Array(usize),
    Iter,
}

impl Display for Access {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Access::Field(s) => write!(f, ".{s}"),
            Access::Array(u) => write!(f, "[{u}]"),
            Access::Iter => write!(f, "[]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ShapeMismatch {
    path: Vec<Access>,
    expected: Shape,
    got: Shape,
}

impl Display for ShapeMismatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Shape mismatch detected!")?;

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
    pub fn new(path: Vec<Access>, expected: Shape, got: Shape) -> Self {
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
            Shape::TVar(t) => write!(f, "<{t}>"),
            Shape::Blob => write!(f, "<>"),
            Shape::Null => write!(f, "<null>"),
            Shape::Bool(None) => write!(f, "<bool>"),
            Shape::Bool(Some(b)) => write!(f, "{b}"),
            Shape::Number(None) => write!(f, "<number>"),
            Shape::Number(Some(n)) => write!(f, "{n}"),
            Shape::String(None) => write!(f, "<string>"),
            Shape::String(Some(s)) => write!(f, "{s}"),
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
            Shape::Mismatch(s1, s2) => write!(f, "({s1} >< {s2})"),
        }
    }
}

struct ShapeContext {
    shapes: HashMap<String, Shape>,
}

impl ShapeContext {
    fn new() -> Self {
        ShapeContext {
            shapes: HashMap::new(),
        }
    }

    fn fresh(&self) -> String {
        format!("T_{}", self.shapes.len())
    }

    fn normalize(&mut self) {
        // 1. topological sort
        // 2. turn all self referential variables into blobs
        // 3. turn all non self referential variables into their actual shape
        let shapes = self.shapes.clone();
        let dependencies = shapes
            .iter()
            .map(|(k, v)| (k.clone(), v.dependencies()))
            .collect::<HashMap<_, _>>();

        let mut sorted = toposort(dependencies);

        while let Some(t) = sorted.pop() {
            let shape = self.shapes.get(&t).unwrap().clone();
            if let Shape::TVar(t_) = shape {
                if t == t_.as_str() {
                    self.shapes.insert(t.to_string(), Shape::Blob);
                } else {
                    let shape = self.shapes.get(&t_).unwrap().clone();
                    self.shapes.insert(t.to_string(), shape);
                }
            } else {
                let shape = shape.normalize(&self.shapes);
                self.shapes.insert(t.to_string(), shape);
            }
        }
    }
}

fn toposort(dependencies: HashMap<String, Vec<String>>) -> TopologicalSort<String> {
    let mut ts = TopologicalSort::<String>::new();
    for (k, v) in dependencies.into_iter() {
        for dep in v.into_iter() {
            if k == dep {
                continue;
            }
            ts.add_dependency(dep, k.clone());
        }
    }
    ts
}
impl Shape {
    pub fn new(f: &Filter) -> Shape {
        let mut ctx = ShapeContext::new();
        ctx.shapes
            .insert("I".to_string(), Shape::TVar("I".to_string()));

        Shape::build_shape(f, vec![Shape::TVar("I".to_string())], &mut ctx);
        ctx.normalize();
        ctx.shapes.remove("I").unwrap()
    }

    pub fn dependencies(&self) -> Vec<String> {
        match self {
            Shape::TVar(t) => vec![t.clone()],
            Shape::Blob => vec![],
            Shape::Null => vec![],
            Shape::Bool(_) => vec![],
            Shape::Number(_) => vec![],
            Shape::String(_) => vec![],
            Shape::Array(shape, _) => shape.dependencies(),
            Shape::Tuple(tuple) => tuple.iter().flat_map(|s| s.dependencies()).collect(),
            Shape::Object(obj) => obj.iter().flat_map(|(_, s)| s.dependencies()).collect(),
            Shape::Mismatch(s1, s2) => s1
                .dependencies()
                .into_iter()
                .chain(s2.dependencies())
                .collect(),
        }
    }

    pub fn normalize(&self, shapes: &HashMap<String, Shape>) -> Shape {
        match self {
            Shape::TVar(t) => {
                let shape = shapes.get(t).unwrap().clone();
                shape.normalize(shapes)
            }
            Shape::Blob => Shape::Blob,
            Shape::Null => Shape::Null,
            Shape::Bool(b) => Shape::Bool(*b),
            Shape::Number(n) => Shape::Number(*n),
            Shape::String(s) => Shape::String(s.clone()),
            Shape::Array(shape, u) => Shape::Array(Box::new(shape.normalize(shapes)), *u),
            Shape::Tuple(tuple) => Shape::Tuple(tuple.iter().map(|s| s.normalize(shapes)).collect()),
            Shape::Object(obj) => Shape::Object(
                obj.iter()
                    .map(|(k, s)| (k.clone(), s.normalize(shapes)))
                    .collect(),
            ),
            Shape::Mismatch(s1, s2) => Shape::Mismatch(
                Box::new(s1.normalize(shapes)),
                Box::new(s2.normalize(shapes)),
            ),
        }
    }

    pub fn from_json(j: Json) -> Shape {
        match j {
            Json::Null => Shape::Null,
            Json::Boolean(b) => Shape::Bool(Some(b)),
            Json::Number(n) => Shape::Number(Some(n)),
            Json::String(s) => Shape::String(Some(s)),
            Json::Array(arr) => Shape::Tuple(arr.into_iter().map(Shape::from_json).collect()),
            Json::Object(obj) => Shape::Object(
                obj.into_iter()
                    .map(|(k, v)| (k, Shape::from_json(v)))
                    .collect(),
            ),
        }
    }

    // pub fn build_shape_pipe(f1: Filter, f2: Filter, s: Shape) -> Shapes {
    //     match f1 {
    //         Filter::Dot => Shape::build_shape(&f2, s),
    //         Filter::Pipe(f11, f12) => {
    //             Shape::build_shape_pipe(*f11, Filter::Pipe(f12, Box::new(f2)), s)
    //         }
    //         Filter::Comma(f11, f12) => {
    //             let s1 = Shape::build_shape_pipe(*f11, f2.clone(), s.clone());
    //             let s2 = Shape::build_shape_pipe(*f12, f2, s);
    //             Shape::merge_shapes(s1, s2)
    //         }
    //         Filter::ObjIndex(s_) => match s {
    //             Shape::Blob => {
    //                 Shape::Object(vec![(s_.clone(), Shape::build_shape(&f2, Shape::Blob))])
    //             }
    //             Shape::Null => Shape::build_shape(&f2, Shape::Null),
    //             Shape::Bool(b) => Shape::build_shape(&f2, Shape::Bool(b)),
    //             Shape::Number(n) => Shape::build_shape(&f2, Shape::Number(n)),
    //             Shape::String(s) => Shape::build_shape(&f2, Shape::String(s)),
    //             Shape::Array(shape, u) => Shape::Mismatch(
    //                 Box::new(Shape::Array(shape, u)),
    //                 Box::new(Shape::Object(vec![(
    //                     s_.clone(),
    //                     Shape::build_shape(&f2, Shape::Blob),
    //                 )])),
    //             ),
    //             Shape::Tuple(tuple) => Shape::Mismatch(
    //                 Box::new(Shape::Tuple(tuple)),
    //                 Box::new(Shape::Object(vec![(
    //                     s_.clone(),
    //                     Shape::build_shape(&f2, Shape::Blob),
    //                 )])),
    //             ),
    //             Shape::Object(mut obj) => {
    //                 let maybe_shape = obj.iter().find_position(|(k, _)| *k == s_);
    //                 if let Some((u, (_, shape))) = maybe_shape {
    //                     obj[u] = (s_.clone(), Shape::build_shape(&f2, shape.clone()));
    //                 } else {
    //                     obj.push((s_.clone(), Shape::build_shape(&f2, Shape::Blob)));
    //                 }

    //                 Shape::Object(obj)
    //             }
    //             Shape::Mismatch(s1, s2) => Shape::Mismatch(
    //                 Box::new(Shape::build_shape(&f2, *s1)),
    //                 Box::new(Shape::build_shape(&f2, *s2)),
    //             ),
    //         },
    //         Filter::ArrayIndex(u) => match s {
    //             Shape::Blob => Shape::Array(Box::new(Shape::build_shape(&f2, s)), Some(u)),
    //             Shape::Null => Shape::build_shape(&f2, Shape::Null),
    //             Shape::Bool(b) => Shape::build_shape(&f2, Shape::Bool(b)),
    //             Shape::Number(n) => Shape::build_shape(&f2, Shape::Number(n)),
    //             Shape::String(s) => Shape::build_shape(&f2, Shape::String(s)),
    //             Shape::Array(shape, u_) => Shape::Array(shape, Some(u.max(u_.unwrap_or(0)))),
    //             Shape::Tuple(tuple) => {
    //                 let mut tuple = tuple.clone();

    //                 while u >= tuple.len() {
    //                     tuple.push(Shape::Blob);
    //                 }

    //                 tuple[u] = Shape::build_shape(&f2, tuple[u].clone());

    //                 Shape::Tuple(tuple)
    //             }
    //             Shape::Object(obj) => Shape::Mismatch(
    //                 Box::new(Shape::Object(obj)),
    //                 Box::new(Shape::Array(Box::new(Shape::Blob), Some(u))),
    //             ),
    //             Shape::Mismatch(s1, s2) => Shape::Mismatch(
    //                 Box::new(Shape::build_shape(&f2, *s1)),
    //                 Box::new(Shape::build_shape(&f2, *s2)),
    //             ),
    //         },
    //         Filter::ArrayIterator => match s {
    //             Shape::Blob => Shape::Array(Box::new(Shape::build_shape(&f2, s)), None),
    //             Shape::Null => Shape::build_shape(&f2, Shape::Null),
    //             Shape::Bool(b) => Shape::build_shape(&f2, Shape::Bool(b)),
    //             Shape::Number(n) => Shape::build_shape(&f2, Shape::Number(n)),
    //             Shape::String(s) => Shape::build_shape(&f2, Shape::String(s)),
    //             Shape::Array(shape, u) => Shape::build_shape(&f2, Shape::Array(shape, u)),
    //             Shape::Tuple(tuple) => Shape::build_shape(&f2, Shape::Tuple(tuple)),
    //             Shape::Object(obj) => Shape::build_shape(&f2, Shape::Object(obj)),
    //             Shape::Mismatch(s1, s2) => Shape::Mismatch(
    //                 Box::new(Shape::build_shape(&f2, *s1)),
    //                 Box::new(Shape::build_shape(&f2, *s2)),
    //             ),
    //         },
    //         Filter::Null => Shape::build_shape(&f2, Shape::Null),
    //         Filter::Boolean(b) => Shape::build_shape(&f2, Shape::Bool(Some(b))),
    //         Filter::Number(n) => Shape::build_shape(&f2, Shape::Number(Some(n))),
    //         Filter::String(s) => Shape::build_shape(&f2, Shape::String(Some(s))),
    //         Filter::Array(arr) => arr
    //             .iter()
    //             .map(|f| Shape::build_shape_pipe(f.clone(), f2.clone(), s.clone()))
    //             .fold(s.clone(), Shape::merge_shapes),
    //         Filter::Object(obj) => obj
    //             .iter()
    //             .flat_map(|(f1, f2)| {
    //                 vec![
    //                     Shape::build_shape_pipe(f1.clone(), f2.clone(), s.clone()),
    //                     Shape::build_shape_pipe(f2.clone(), f2.clone(), s.clone()),
    //                 ]
    //             })
    //             .fold(s.clone(), Shape::merge_shapes),
    //         Filter::BinOp(lhs, op, rhs) => {
    //             let s1 = Shape::build_shape_pipe(*lhs.clone(), f2.clone(), s.clone());
    //             let s2 = Shape::build_shape_pipe(*rhs.clone(), f2, s);
    //             Shape::merge_shapes(s1, s2)
    //         }
    //         Filter::Empty => todo!(),
    //         Filter::Error(_) => todo!(),
    //     }
    // }

    pub fn build_shape(f: &Filter, shapes: Vec<Shape>, ctx: &mut ShapeContext) -> Vec<Shape> {
        match f {
            Filter::Dot => shapes,
            Filter::Pipe(f1, f2) => {
                let shapes = Shape::build_shape(f1, shapes, ctx);
                Shape::build_shape(f2, shapes, ctx)
            }
            Filter::Comma(f1, f2) => {
                let s1 = Shape::build_shape(f1, shapes.clone(), ctx);
                let s2 = Shape::build_shape(f2, shapes, ctx);
                [s1, s2].concat()
            }
            Filter::ObjIndex(s) => shapes
                .into_iter()
                .map(|shape| match shape {
                    Shape::Blob => {
                        let new_type_var = ctx.fresh();
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));
                        Shape::TVar(new_type_var)
                    }
                    Shape::TVar(t) => {
                        let new_type_var = ctx.fresh();
                        let current_shape = ctx.shapes.get(&t).unwrap().clone();
                        // <'I> + {s: <'T>}
                        let current_shape = Shape::merge_shapes(
                            current_shape,
                            Shape::Object(vec![(s.clone(), Shape::TVar(new_type_var.clone()))]),
                            ctx,
                        );
                        ctx.shapes.insert(t, current_shape);
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));
                        Shape::TVar(new_type_var)
                    }
                    Shape::Null => Shape::Null,
                    Shape::Bool(b) => Shape::Mismatch(
                        Box::new(Shape::Bool(b)),
                        Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                    ),
                    Shape::Number(n) => Shape::Mismatch(
                        Box::new(Shape::Number(n)),
                        Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                    ),
                    Shape::String(s_) => Shape::Mismatch(
                        Box::new(Shape::String(s_)),
                        Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                    ),
                    Shape::Array(arr, u) => Shape::Mismatch(
                        Box::new(Shape::Array(arr, u)),
                        Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                    ),
                    Shape::Tuple(tuple) => Shape::Mismatch(
                        Box::new(Shape::Tuple(tuple)),
                        Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                    ),
                    Shape::Object(mut obj) => {
                        let has_key = obj.iter().any(|(k, _)| k == s);

                        let new_type_var = ctx.fresh();

                        if !has_key {
                            obj.push((s.clone(), Shape::TVar(new_type_var)));
                        }

                        Shape::Object(obj)
                    }
                    Shape::Mismatch(s1, s2) => todo!(),
                })
                .collect(),
            Filter::ArrayIndex(u) => shapes
                .into_iter()
                .map(|shape| match shape {
                    Shape::Blob => {
                        let new_type_var = ctx.fresh();
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));
                        Shape::TVar(new_type_var)
                    }
                    Shape::TVar(t) => {
                        let new_type_var = ctx.fresh();
                        let current_shape = ctx.shapes.get(&t).unwrap().clone();
                        // <'I> + {s: <'T>}
                        let current_shape = Shape::merge_shapes(
                            current_shape,
                            Shape::Array(Box::new(Shape::TVar(new_type_var.clone())), Some(*u)),
                            ctx,
                        );

                        ctx.shapes.insert(t, current_shape);
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));

                        Shape::TVar(new_type_var)
                    }
                    Shape::Null => Shape::Null,
                    Shape::Bool(b) => Shape::Mismatch(
                        Box::new(Shape::Bool(b)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                    ),
                    Shape::Number(n) => Shape::Mismatch(
                        Box::new(Shape::Number(n)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                    ),
                    Shape::String(s) => Shape::Mismatch(
                        Box::new(Shape::String(s)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                    ),
                    Shape::Array(shape, None) => Shape::Array(shape, Some(*u)),
                    Shape::Array(shape, Some(u_)) => Shape::Array(shape, Some(*u.max(&u_))),
                    Shape::Tuple(mut tuple) => {
                        while *u >= tuple.len() {
                            let new_type_var = ctx.fresh();
                            tuple.push(Shape::TVar(new_type_var));
                        }

                        Shape::Tuple(tuple)
                    }
                    Shape::Object(obj) => Shape::Mismatch(
                        Box::new(Shape::Object(obj)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                    ),
                    Shape::Mismatch(s1, s2) => todo!(),
                })
                .collect(),
            Filter::ArrayIterator => shapes
                .into_iter()
                .map(|shape| match shape {
                    Shape::Blob => {
                        let new_type_var = ctx.fresh();
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));
                        vec![Shape::TVar(new_type_var)]
                    }
                    Shape::TVar(t) => {
                        let new_type_var = ctx.fresh();
                        let current_shape = ctx.shapes.get(&t).unwrap().clone();
                        let current_shape = Shape::merge_shapes(
                            current_shape,
                            Shape::Array(Box::new(Shape::TVar(new_type_var.clone())), None),
                            ctx,
                        );
                        ctx.shapes.insert(t, current_shape);
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));

                        vec![Shape::TVar(new_type_var)]
                    }
                    Shape::Null => vec![Shape::Mismatch(
                        Box::new(Shape::Null),
                        Box::new(Shape::Array(Box::new(Shape::Blob), None)),
                    )],
                    Shape::Bool(b) => vec![Shape::Mismatch(
                        Box::new(Shape::Bool(b)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), None)),
                    )],
                    Shape::Number(n) => vec![Shape::Mismatch(
                        Box::new(Shape::Number(n)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), None)),
                    )],
                    Shape::String(s) => vec![Shape::Mismatch(
                        Box::new(Shape::String(s)),
                        Box::new(Shape::Array(Box::new(Shape::Blob), None)),
                    )],
                    Shape::Array(shape, u) => vec![*shape.clone()],
                    Shape::Tuple(vec) => vec,
                    Shape::Object(vec) => vec.into_iter().map(|(_, shape)| shape).collect(),
                    Shape::Mismatch(s1, s2) => todo!(),
                })
                .flatten()
                .collect(),
            Filter::Null => vec![Shape::Null],
            Filter::Boolean(b) => vec![Shape::Bool(Some(*b))],
            Filter::Number(n) => vec![Shape::Number(Some(*n))],
            Filter::String(s) => vec![Shape::String(Some(s.clone()))],
            Filter::Array(vec) => shapes // todo: check this
                .into_iter()
                .map(|shape| {
                    // number, string
                    let shapes_: Vec<Shape> = vec
                        .iter()
                        // todo: could we just pass shapes inside????
                        .map(|f| Shape::build_shape(f, vec![shape.clone()], ctx))
                        .flatten()
                        .collect();

                    Shape::Tuple(shapes_)
                })
                .collect(),
            Filter::Object(vec) => shapes
                .into_iter()
                .map(|shape| {
                    Shape::Object(
                        vec.iter()
                            .map(|(f1, f2)| {
                                (
                                    Shape::build_shape(f1, vec![shape.clone()], ctx)
                                        .first()
                                        .expect("expect 1 shape only")
                                        .to_string(),
                                    Shape::build_shape(f2, vec![shape.clone()], ctx)
                                        .first()
                                        .expect("expect 1 shape only")
                                        .clone(),
                                )
                            })
                            .collect(),
                    )
                })
                .collect(),
            Filter::BinOp(l, op, r) => match op {
                crate::BinOp::Add => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx);
                    let s2 = Shape::build_shape(r, shapes, ctx);

                    s1.into_iter()
                        .zip(s2.into_iter())
                        .fold(vec![], |mut acc, (l, r)| {
                            match (l, r) {
                                // null + X
                                // X + null
                                (Shape::Null, s) | (s, Shape::Null) => {
                                    acc.push(s);
                                    acc
                                }
                                // str + str
                                (Shape::String(s1), Shape::String(s2)) => {
                                    match (s1, s2) {
                                        (None, None) => acc.push(Shape::String(None)),
                                        (Some(s1), None) | (None, Some(s1)) => {
                                            acc.push(Shape::String(Some(s1)))
                                        }
                                        (Some(s1), Some(s2)) => {
                                            acc.push(Shape::String(Some(format!("{s1}{s2}"))));
                                        }
                                    }
                                    acc
                                }
                                (Shape::String(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::String(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::String(None),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    acc.push(Shape::String(None));
                                    acc
                                }
                                // arr + arr
                                (Shape::Array(shape1, u1), Shape::Array(shape2, u2)) => {
                                    let shape = Shape::merge_shapes(*shape1, *shape2, ctx);
                                    let u = u1.max(u2);

                                    acc.push(Shape::Array(Box::new(shape), u));
                                    acc
                                }
                                (Shape::Array(shape, u), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Array(shape, u)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Array(shape.clone(), u),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    acc.push(Shape::Array(shape, u));
                                    acc
                                }
                                // obj + obj
                                (Shape::Object(obj1), Shape::Object(obj2)) => {
                                    // todo: check this
                                    let mut obj = HashMap::new();

                                    for (k, s1) in obj1.clone() {
                                        let s2 =
                                            obj2.iter().find(|(key, _)| key == &k).map(|(_, s)| s);

                                        if let Some(s2) = s2 {
                                            obj.insert(k, Shape::merge_shapes(s1, s2.clone(), ctx));
                                        } else {
                                            obj.insert(k, s1);
                                        }
                                    }

                                    for (k, s2) in obj2 {
                                        let s1 =
                                            obj1.iter().find(|(key, _)| key == &k).map(|(_, s)| s);

                                        if let Some(s1) = s1 {
                                            obj.insert(k, Shape::merge_shapes(s1.clone(), s2, ctx));
                                        } else {
                                            obj.insert(k, s2);
                                        }
                                    }

                                    acc.push(Shape::Object(obj.into_iter().collect()));
                                    acc
                                }
                                (Shape::Object(obj), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Object(obj)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Object(obj.clone()),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    acc.push(Shape::Object(obj));
                                    acc
                                }
                                // num + num
                                (Shape::Number(n1), Shape::Number(n2)) => {
                                    match (n1, n2) {
                                        (None, None) => acc.push(Shape::Number(None)),
                                        (Some(n1), None) | (None, Some(n1)) => {
                                            acc.push(Shape::Number(Some(n1)))
                                        }
                                        (Some(n1), Some(n2)) => {
                                            acc.push(Shape::Number(Some(n1 + n2)));
                                        }
                                    }
                                    acc
                                }
                                (Shape::Number(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Number(None),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    acc.push(Shape::Number(None));
                                    acc
                                }
                                (l, r) => {
                                    acc.push(Shape::Mismatch(Box::new(l), Box::new(r)));
                                    acc
                                }
                            }
                        })
                }
                crate::BinOp::Sub
                | crate::BinOp::Mul
                | crate::BinOp::Div
                | crate::BinOp::Gt
                | crate::BinOp::Ge
                | crate::BinOp::Lt
                | crate::BinOp::Le
                | crate::BinOp::Mod => {
                    todo!()
                }
                crate::BinOp::And | crate::BinOp::Or => {
                    todo!()
                }
                crate::BinOp::Eq | crate::BinOp::Ne => {
                    todo!()
                }
            },
            Filter::Empty => todo!(),
            Filter::Error(_) => todo!(),
        }
    }

    pub fn merge_shapes(s1: Shape, s2: Shape, ctx: &mut ShapeContext) -> Shape {
        match (s1, s2) {
            (Shape::TVar(t), s) | (s, Shape::TVar(t)) => {
                let current_shape = ctx.shapes.get(&t).unwrap().clone();
                let new_shape = match current_shape {
                    Shape::Blob => {
                        ctx.shapes.insert(t.clone(), s.clone());
                        s
                    }
                    Shape::TVar(t1) => {
                        // t = t1
                        // t1 = X
                        ctx.shapes.insert(t1, s.clone());
                        s
                    }
                    _ => {
                        let current_shape = Shape::merge_shapes(current_shape, s.clone(), ctx);
                        ctx.shapes.insert(t.clone(), current_shape.clone());
                        current_shape
                    }
                };

                new_shape
            }
            (Shape::Null, Shape::Null) => Shape::Null,
            (Shape::Bool(b1), Shape::Bool(b2)) => Shape::Bool(b1.or(b2)),
            (Shape::Number(n1), Shape::Number(n2)) => Shape::Number(n1.or(n2)),
            (Shape::String(s1), Shape::String(s2)) => Shape::String(s1.or(s2)),
            (Shape::Array(shape1, u1), Shape::Array(shape2, u2)) => Shape::Array(
                Box::new(Shape::merge_shapes(*shape1, *shape2, ctx)),
                u1.max(u2),
            ),
            (Shape::Tuple(mut tuple1), Shape::Tuple(mut tuple2)) => {
                let len = tuple1.len().max(tuple2.len());

                while tuple1.len() < len {
                    let new_type_var = ctx.fresh();
                    tuple1.push(Shape::TVar(new_type_var));
                }

                while tuple2.len() < len {
                    let new_type_var = ctx.fresh();
                    tuple2.push(Shape::TVar(new_type_var));
                }

                let tuple = tuple1
                    .into_iter()
                    .zip(tuple2)
                    .map(|(s1, s2)| Shape::merge_shapes(s1, s2, ctx))
                    .collect();

                Shape::Tuple(tuple)
            }
            (Shape::Object(obj1), Shape::Object(obj2)) => {
                let mut obj = HashMap::new();

                for (k, s1) in obj1.clone() {
                    let s2 = obj2.iter().find(|(key, _)| key == &k).map(|(_, s)| s);

                    if let Some(s2) = s2 {
                        obj.insert(k, Shape::merge_shapes(s1, s2.clone(), ctx));
                    } else {
                        obj.insert(k, s1);
                    }
                }

                for (k, s2) in obj2 {
                    let s1 = obj1.iter().find(|(key, _)| key == &k).map(|(_, s)| s);

                    if let Some(s1) = s1 {
                        obj.insert(k, Shape::merge_shapes(s1.clone(), s2, ctx));
                    } else {
                        obj.insert(k, s2);
                    }
                }

                Shape::Object(obj.into_iter().collect())
            }
            (Shape::Mismatch(s1, s2), Shape::Mismatch(s3, s4)) => Shape::Mismatch(
                Box::new(Shape::merge_shapes(*s1, *s3, ctx)),
                Box::new(Shape::merge_shapes(*s2, *s4, ctx)),
            ),
            (Shape::Mismatch(s1, s2), s) => {
                Shape::Mismatch(Box::new(Shape::merge_shapes(*s1, s.clone(), ctx)), s2)
            }
            (s, Shape::Mismatch(s1, s2)) => {
                Shape::Mismatch(s1, Box::new(Shape::merge_shapes(*s2, s, ctx)))
            }
            (s1, s2) => Shape::Mismatch(Box::new(s1), Box::new(s2)),
        }
    }

    pub fn check(&self, j: Json, path: Vec<Access>) -> Option<ShapeMismatch> {
        match self {
            Shape::Blob => None,
            Shape::TVar(_) => None,
            Shape::Null => {
                if let Json::Null = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Bool(None) => {
                if let Json::Boolean(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Bool(Some(b)) => {
                if let Json::Boolean(b_) = j {
                    if b == &b_ {
                        None
                    } else {
                        Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Number(None) => {
                if let Json::Number(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Number(Some(n)) => {
                if let Json::Number(n_) = j {
                    if n == &n_ {
                        None
                    } else {
                        Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::String(None) => {
                if let Json::String(_) = j {
                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::String(Some(s)) => {
                if let Json::String(s_) = &j {
                    if s == s_ {
                        None
                    } else {
                        Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                    }
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Array(shape, u) => {
                if let Json::Array(arr) = j {
                    if let Some(u) = u {
                        if *u > arr.len() {
                            return Some(ShapeMismatch::new(
                                path,
                                self.clone(),
                                Shape::Tuple(arr.into_iter().map(Shape::from_json).collect()),
                            ));
                        }
                    }

                    let (_, mismatches): (Vec<_>, Vec<_>) = arr
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
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Tuple(tuple) => {
                if let Json::Array(arr) = j {
                    if tuple.len() > arr.len() {
                        return Some(ShapeMismatch::new(
                            path,
                            self.clone(),
                            Shape::Tuple(arr.into_iter().map(Shape::from_json).collect()),
                        ));
                    }

                    let (_, mismatches): (Vec<_>, Vec<_>) = tuple
                        .iter()
                        .enumerate()
                        .zip(arr)
                        .map(|((i, s), j)| {
                            Shape::check(s, j, [path.clone(), vec![Access::Array(i)]].concat())
                        })
                        .partition(Option::is_none);

                    mismatches.into_iter().next().flatten()
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Object(obj_shape) => {
                if let Json::Object(obj) = j {
                    let obj_map = obj.clone().into_iter().collect::<HashMap<String, Json>>();

                    let (_, mismatches): (Vec<_>, Vec<_>) = obj_shape
                        .iter()
                        .map(|(key, shape)| {
                            let v = obj_map.get(key);

                            if let Some(v) = v {
                                Shape::check(
                                    shape,
                                    v.clone(),
                                    [path.clone(), vec![Access::Field(key.to_string())]].concat(),
                                )
                            } else {
                                Some(ShapeMismatch::new(
                                    path.clone(),
                                    self.clone(),
                                    Shape::Object(
                                        obj.clone()
                                            .into_iter()
                                            .map(|(k, v)| (k, Shape::from_json(v)))
                                            .collect(),
                                    ),
                                ))
                            }
                        })
                        .partition(Option::is_none);

                    mismatches.into_iter().next().flatten()
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Mismatch(s1, s2) => Some(ShapeMismatch::new(path, *s1.clone(), *s2.clone())),
        }
    }

    pub fn check_self(&self, path: Vec<Access>) -> Option<ShapeMismatch> {
        match self {
            Shape::Blob
            | Shape::TVar(_)
            | Shape::Null
            | Shape::Bool(..)
            | Shape::Number(..)
            | Shape::String(..) => None,
            Shape::Array(shape, _) => shape.check_self([path.clone(), vec![Access::Iter]].concat()),
            Shape::Tuple(tuple) => {
                let (_, mismatches): (Vec<_>, Vec<_>) = tuple
                    .iter()
                    .enumerate()
                    .map(|(i, s)| s.check_self([path.clone(), vec![Access::Array(i)]].concat()))
                    .partition(Option::is_none);

                mismatches.into_iter().next().flatten()
            }
            Shape::Object(obj) => {
                let (_, mismatches): (Vec<_>, Vec<_>) = obj
                    .iter()
                    .map(|(key, shape)| {
                        shape.check_self(
                            [path.clone(), vec![Access::Field(key.to_string())]].concat(),
                        )
                    })
                    .partition(Option::is_none);

                mismatches.into_iter().next().flatten()
            }
            Shape::Mismatch(s1, s2) => Some(ShapeMismatch::new(path, *s1.clone(), *s2.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{BinOp, Filter, Json, Shape, ShapeMismatch};

    #[test]
    fn test_plus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Add,
            Box::new(Filter::Dot),
        );
        println!("{filter}");
        let shape = Shape::new(&filter);
        println!("{shape}");
        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    #[test]
    fn test_false_plus() {
        let json = Json::Number(1.0);
        let filter = Filter::Comma(
            Box::new(Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Add,
                Box::new(Filter::Number(1.0)),
            )),
            Box::new(Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Add,
                Box::new(Filter::String("hello".to_string())),
            )),
        );
        println!("{filter}");
        let shape = Shape::new(&filter);
        let mismatch = shape.check_self(vec![]);
        println!("{shape}");
        assert_eq!(
            mismatch,
            Some(ShapeMismatch::new(
                vec![],
                Shape::Number(None),
                Shape::String(None)
            ))
        );
    }

    #[test]
    fn test_minus() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Sub,
            Box::new(Filter::Number(2.0)),
        );
        println!("{filter}");
        let results = Filter::filter(&json, &filter);

        assert_eq!(results, vec![Ok(Json::Number(-1.0))]);
    }
}
