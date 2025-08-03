use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter, Json, UnOp};
use topological_sort::TopologicalSort;

#[derive(Debug, Clone, PartialEq)]
pub enum Shape {
    TVar(usize),
    Blob,
    Null,
    Bool(Option<bool>),
    Number(Option<f64>),
    String(Option<String>),
    Array(Box<Shape>, Option<isize>),
    Tuple(Vec<Shape>),
    Object(Vec<(String, Shape)>),
    Mismatch(Box<Shape>, Box<Shape>),
    Union(Box<Shape>, Box<Shape>),
}

pub enum Type {
    Arrow(Box<Shape>, Box<Shape>), // A -> B
}

// . : |- A -> A
// 3 : |- A -> number<3>
// f1 | f2 : (f1 : A -> B), (f2 : A -> C) |- (A -> C)

// 3 + .
// 3 + . : number
// 3 + . : (null -> number<3>) | (number -> number)
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
        writeln!(f, "[x] shape mismatch detected!")?;

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
            Shape::TVar(t) => write!(f, "<T_{t}>"),
            Shape::Blob => write!(f, "<>"),
            Shape::Null => write!(f, "<null>"),
            Shape::Bool(None) => write!(f, "<bool>"),
            Shape::Bool(Some(b)) => write!(f, "{b}"),
            Shape::Number(None) => write!(f, "<number>"),
            Shape::Number(Some(n)) => write!(f, "{n}"),
            Shape::String(None) => write!(f, "<string>"),
            Shape::String(Some(s)) => write!(f, "\"{s}\""),
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
            Shape::Union(s1, s2) => write!(f, "({s1} | {s2})"),
        }
    }
}

#[derive(Debug)]
pub struct ShapeContext {
    pub shapes: HashMap<usize, Shape>,
}

impl ShapeContext {
    fn new() -> Self {
        ShapeContext {
            shapes: HashMap::new(),
        }
    }

    fn fresh(&self) -> usize {
        self.shapes.len()
    }

    fn normalize(&mut self) {
        // 1. topological sort
        // 2. turn all self referential variables into blobs
        // 3. turn all non self referential variables into their actual shape

        self.shapes.iter_mut().for_each(|(k, v)| {
            if let Shape::TVar(t) = v {
                tracing::debug!("Checking {t} == {k}");
                if t == k {
                    *v = Shape::Blob;
                }
            }
        });

        let shapes = self.shapes.clone();

        let dependencies = shapes
            .iter()
            .map(|(k, v)| (k.clone(), v.dependencies()))
            .collect::<HashMap<_, _>>();

        let mut sorted = toposort(dependencies);

        while let Some(t) = sorted.pop() {
            let shape = self.shapes.get(&t).unwrap().clone();
            if let Shape::TVar(t_) = shape {
                tracing::debug!("Normalizing {t} -> {t_}");
                if t == t_ {
                    self.shapes.insert(t, Shape::Blob);
                } else {
                    let shape = self.shapes.get(&t_).unwrap().clone();
                    self.shapes.insert(t, shape);
                }
            } else {
                let shape = shape.normalize(&self.shapes);
                self.shapes.insert(t, shape);
            }
        }
    }
}

fn toposort(dependencies: HashMap<usize, Vec<usize>>) -> TopologicalSort<usize> {
    let mut ts = TopologicalSort::<usize>::new();
    for (k, v) in dependencies.into_iter() {
        for dep in v.into_iter() {
            ts.add_dependency(dep, k);
        }
    }
    ts
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Subtyping {
    Subtype,
    Supertype,
    Incompatible,
}

impl Shape {
    pub fn new(f: &Filter, filters: &HashMap<String, Filter>) -> (Shape, Vec<Shape>) {
        let mut ctx = ShapeContext::new();
        ctx.shapes.insert(0, Shape::TVar(0));

        let mut results = Shape::build_shape(f, vec![Shape::TVar(0)], &mut ctx, filters);
        tracing::debug!("type context: {:?}", ctx.shapes);
        tracing::debug!("result types: {:?}", results);
        ctx.normalize();

        for result in results.iter_mut() {
            *result = result.normalize(&ctx.shapes).canonicalize();
        }

        (ctx.shapes.remove(&0).unwrap(), results)
    }

    pub fn dependencies(&self) -> Vec<usize> {
        match self {
            Shape::TVar(t) => vec![*t],
            Shape::Blob => vec![],
            Shape::Null => vec![],
            Shape::Bool(_) => vec![],
            Shape::Number(_) => vec![],
            Shape::String(_) => vec![],
            Shape::Array(shape, _) => shape.dependencies(),
            Shape::Tuple(tuple) => tuple.iter().flat_map(|s| s.dependencies()).collect(),
            Shape::Object(obj) => obj.iter().flat_map(|(_, s)| s.dependencies()).collect(),
            Shape::Mismatch(s1, s2) | Shape::Union(s1, s2) => s1
                .dependencies()
                .into_iter()
                .chain(s2.dependencies())
                .collect(),
        }
    }

    pub fn normalize(&self, shapes: &HashMap<usize, Shape>) -> Shape {
        tracing::debug!("normalizing {self}");
        match self {
            Shape::TVar(t) => {
                let shape = shapes.get(t).unwrap().clone();
                if let Shape::TVar(t_) = &shape {
                    if t == t_ {
                        Shape::Blob
                    } else {
                        shape.normalize(shapes)
                    }
                } else {
                    shape.normalize(shapes)
                }
            }
            Shape::Blob => Shape::Blob,
            Shape::Null => Shape::Null,
            Shape::Bool(b) => Shape::Bool(*b),
            Shape::Number(n) => Shape::Number(*n),
            Shape::String(s) => Shape::String(s.clone()),
            Shape::Array(shape, u) => Shape::Array(Box::new(shape.normalize(shapes)), *u),
            Shape::Tuple(tuple) => {
                Shape::Tuple(tuple.iter().map(|s| s.normalize(shapes)).collect())
            }
            Shape::Object(obj) => Shape::Object(
                obj.iter()
                    .map(|(k, s)| (k.clone(), s.normalize(shapes)))
                    .collect(),
            ),
            Shape::Mismatch(s1, s2) => Shape::Mismatch(
                Box::new(s1.normalize(shapes)),
                Box::new(s2.normalize(shapes)),
            ),
            Shape::Union(s1, s2) => Shape::Union(
                Box::new(s1.normalize(shapes)),
                Box::new(s2.normalize(shapes)),
            ),
        }
    }

    pub fn subtype(&self, other: &Self) -> Subtyping {
        match (self, other) {
            (Shape::TVar(t), _) | (_, Shape::TVar(t)) => {
                // TVar can be any type, so it should not be a subtype of any other type
                Subtyping::Incompatible
            }
            (Shape::Null, Shape::Null) => Subtyping::Subtype,
            (Shape::Blob, Shape::Blob) => Subtyping::Subtype,
            (Shape::Bool(b1), Shape::Bool(b2)) => match (b1, b2) {
                (Some(b1), Some(b2)) if b1 == b2 => Subtyping::Subtype,
                (Some(_), None) => Subtyping::Supertype,
                (None, Some(_)) => Subtyping::Subtype,
                (None, None) => Subtyping::Subtype,
                _ => Subtyping::Incompatible,
            },
            (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                (Some(n1), Some(n2)) if n1 == n2 => Subtyping::Subtype,
                (Some(_), None) => Subtyping::Supertype,
                (None, Some(_)) => Subtyping::Subtype,
                (None, None) => Subtyping::Subtype,
                _ => Subtyping::Incompatible,
            },
            (Shape::String(s1), Shape::String(s2)) => match (s1, s2) {
                (Some(s1), Some(s2)) if s1 == s2 => Subtyping::Subtype,
                (Some(_), None) => Subtyping::Supertype,
                (None, Some(_)) => Subtyping::Subtype,
                (None, None) => Subtyping::Subtype,
                _ => Subtyping::Incompatible,
            },
            (Shape::Array(shape1, u1), Shape::Array(shape2, u2)) => {
                let inner_subtyping = shape1.subtype(shape2);
                let outer_subtyping = match (u1, u2) {
                    // Larger arrays can be used in place of smaller arrays, so the smaller array is the supertype
                    (Some(u1), Some(u2)) => match u1.cmp(u2) {
                        Ordering::Less => Subtyping::Supertype,
                        Ordering::Greater => Subtyping::Subtype,
                        Ordering::Equal => Subtyping::Subtype,
                    },
                    (None, Some(_)) => Subtyping::Supertype,
                    (Some(_), None) => Subtyping::Subtype,
                    (None, None) => Subtyping::Subtype,
                };

                match (inner_subtyping, outer_subtyping) {
                    (Subtyping::Subtype, Subtyping::Subtype) => Subtyping::Subtype,
                    (Subtyping::Supertype, Subtyping::Supertype) => Subtyping::Supertype,
                    (Subtyping::Incompatible, _)
                    | (_, Subtyping::Incompatible)
                    | (Subtyping::Subtype, Subtyping::Supertype)
                    | (Subtyping::Supertype, Subtyping::Subtype) => Subtyping::Incompatible,
                }
            }
            (Shape::Array(shape, u), Shape::Tuple(shapes)) => {
                // If `shape` is a subtype of all elements in `shapes`
                // and `u` is greater than `shapes.len()`, then array is a subtype of the tuple
                // because for any place that expects the tuple, we can use the array instead.
                let inner_subtyping = shapes.iter().all(|s| {
                    if let Subtyping::Subtype = shape.subtype(s) {
                        true
                    } else {
                        false
                    }
                });

                if inner_subtyping && u.unwrap_or(0).max(0) as usize >= shapes.len() {
                    return Subtyping::Subtype;
                }

                // If `shape` is a supertype of all elements in `shapes`
                // and `u` is less than `shapes.len()`, then array is a supertype of the tuple
                // because for any place that expects the array, we can use the tuple instead.
                let inner_subtyping = shapes.iter().all(|s| {
                    if let Subtyping::Supertype = shape.subtype(s) {
                        true
                    } else {
                        false
                    }
                });

                if inner_subtyping && u.unwrap_or(0).max(0) as usize <= shapes.len() {
                    return Subtyping::Supertype;
                }

                Subtyping::Incompatible
            }
            (Shape::Tuple(shapes), Shape::Array(shape, u)) => {
                todo!("should be the reverse of the above")
            }
            (Shape::Tuple(shapes1), Shape::Tuple(shapes2)) => {
                let subtypings = shapes1
                    .iter()
                    .zip(shapes2)
                    .map(|(s1, s2)| s1.subtype(s2))
                    .collect::<Vec<_>>();

                if subtypings.iter().all(|s| *s == Subtyping::Subtype)
                    && shapes1.len() >= shapes2.len()
                {
                    Subtyping::Subtype
                } else if subtypings.iter().all(|s| *s == Subtyping::Supertype)
                    && shapes1.len() <= shapes2.len()
                {
                    Subtyping::Supertype
                } else {
                    Subtyping::Incompatible
                }
            }
            (Shape::Object(obj1), Shape::Object(obj2)) => {
                let mut subtypings = vec![];

                for (k, s1) in obj1 {
                    let s2 = obj2.iter().find(|(key, _)| key == k).map(|(_, s)| s);

                    if let Some(s2) = s2 {
                        subtypings.push(s1.subtype(s2));
                    } else {
                        subtypings.push(Subtyping::Subtype);
                    }
                }

                for (k, s2) in obj2 {
                    let s1 = obj1.iter().find(|(key, _)| key == k).map(|(_, s)| s);

                    if let Some(s1) = s1 {
                        subtypings.push(s1.subtype(s2));
                    } else {
                        subtypings.push(Subtyping::Supertype);
                    }
                }

                if subtypings.iter().all(|s| *s == Subtyping::Subtype) {
                    Subtyping::Subtype
                } else if subtypings.iter().all(|s| *s == Subtyping::Supertype) {
                    Subtyping::Supertype
                } else {
                    Subtyping::Incompatible
                }
            }
            (Shape::Mismatch(s1, s2), _) | (_, Shape::Mismatch(s1, s2)) => Subtyping::Incompatible,
            (Shape::Union(s1, s2), s) | (s, Shape::Union(s1, s2)) => {
                match (s1.subtype(s), s2.subtype(s)) {
                    (Subtyping::Subtype, Subtyping::Subtype) => Subtyping::Subtype,
                    (Subtyping::Supertype, Subtyping::Supertype) => Subtyping::Supertype,
                    _ => Subtyping::Incompatible,
                }
            }
            _ => Subtyping::Incompatible,
        }
    }

    pub fn canonicalize(&self) -> Shape {
        match self {
            Shape::TVar(_)
            | Shape::Blob
            | Shape::Null
            | Shape::Bool(_)
            | Shape::Number(_)
            | Shape::String(_) => self.clone(),
            Shape::Array(shape, u) => Shape::Array(Box::new(shape.canonicalize()), *u),
            Shape::Tuple(shapes) => {
                Shape::Tuple(shapes.iter().map(|s| s.canonicalize()).collect::<Vec<_>>())
            }
            Shape::Object(items) => Shape::Object(
                items
                    .iter()
                    .map(|(k, v)| (k.clone(), v.canonicalize()))
                    .collect::<Vec<_>>(),
            ),
            Shape::Mismatch(s1, s2) => {
                Shape::Mismatch(Box::new(s1.canonicalize()), Box::new(s2.canonicalize()))
            }
            Shape::Union(s1, s2) => match (*s1.clone(), *s2.clone()) {
                (Shape::Union(s2, s3), s1) | (s1, Shape::Union(s2, s3)) => {
                    let s1 = s1.canonicalize();
                    let s2 = s2.canonicalize();
                    let s3 = s3.canonicalize();
                    match (s1.subtype(&s2), s1.subtype(&s3)) {
                        (Subtyping::Subtype, _) | (_, Subtyping::Subtype) => {
                            Shape::Union(Box::new(s2), Box::new(s3)).canonicalize()
                        }
                        (Subtyping::Supertype, _) => {
                            Shape::Union(Box::new(s1), Box::new(s3)).canonicalize()
                        }
                        (_, Subtyping::Supertype) => {
                            Shape::Union(Box::new(s1), Box::new(s2)).canonicalize()
                        }
                        (Subtyping::Incompatible, Subtyping::Incompatible) => Shape::Union(
                            Box::new(s1),
                            Box::new(Shape::Union(Box::new(s2), Box::new(s3))),
                        ),
                    }
                }
                (s1, s2) => {
                    let s1 = s1.canonicalize();
                    let s2 = s2.canonicalize();
                    match s1.subtype(&s2) {
                        Subtyping::Subtype => s2,
                        Subtyping::Supertype => s1,
                        Subtyping::Incompatible => Shape::Union(Box::new(s1), Box::new(s2)),
                    }
                }
            },
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

    pub fn build_shape(
        f: &Filter,
        shapes: Vec<Shape>,
        ctx: &mut ShapeContext,
        filters: &HashMap<String, Filter>,
    ) -> Vec<Shape> {
        match f {
            Filter::Dot => shapes,
            Filter::Pipe(f1, f2) => {
                let shapes = Shape::build_shape(f1, shapes, ctx, filters);
                Shape::build_shape(f2, shapes, ctx, filters)
            }
            Filter::Comma(f1, f2) => {
                let s1 = Shape::build_shape(f1, shapes.clone(), ctx, filters);
                let s2 = Shape::build_shape(f2, shapes, ctx, filters);
                [s1, s2].concat()
            }
            Filter::ObjIndex(s) => shapes
                .into_iter()
                .map(|shape| match shape {
                    Shape::Blob => {
                        let new_type_var = ctx.fresh();
                        ctx.shapes
                            .insert(new_type_var.clone(), Shape::TVar(new_type_var.clone()));
                        // todo: check this
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
                    Shape::Union(s1, s2) => todo!(),
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
                        while (*u).max(0) as usize >= tuple.len() {
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
                    Shape::Union(s1, s2) => todo!(),
                })
                .collect(),
            Filter::ArrayIterator => shapes
                .into_iter()
                .flat_map(|shape| match shape {
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
                    Shape::Union(s1, s2) => todo!(),
                })
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
                        .flat_map(|f| Shape::build_shape(f, vec![shape.clone()], ctx, filters))
                        .collect();
                    let merged = shapes_.iter().fold(Shape::Blob, |acc, s| {
                        Shape::merge_shapes(acc, s.clone(), ctx)
                    });
                    if let Shape::Mismatch(_, _) = merged {
                        Shape::Tuple(shapes_)
                    } else {
                        Shape::Array(Box::new(merged), None)
                    }
                })
                .collect(),
            Filter::Object(vec) => shapes
                .into_iter()
                .map(|shape| {
                    Shape::Object(
                        vec.iter()
                            .map(|(f1, f2)| {
                                (
                                    Shape::build_shape(f1, vec![shape.clone()], ctx, filters)
                                        .first()
                                        .expect("expect 1 shape only")
                                        .to_string(),
                                    Shape::build_shape(f2, vec![shape.clone()], ctx, filters)
                                        .first()
                                        .expect("expect 1 shape only")
                                        .clone(),
                                )
                            })
                            .collect(),
                    )
                })
                .collect(),
            Filter::UnOp(op, filter) => match op {
                UnOp::Not => {
                    let s = Shape::build_shape(filter, shapes, ctx, filters);
                    s.into_iter()
                        .map(|s| match s {
                            Shape::Bool(b) => Shape::Bool(b.map(|b| !b)),
                            Shape::TVar(t) => {
                                let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                let current_shape =
                                    Shape::merge_shapes(current_shape, Shape::Bool(None), ctx);
                                ctx.shapes.insert(t, current_shape);

                                Shape::Bool(None)
                            }
                            s => Shape::Mismatch(Box::new(s), Box::new(Shape::Bool(None))),
                        })
                        .collect()
                }
                UnOp::Neg => {
                    let s = Shape::build_shape(filter, shapes, ctx, filters);
                    s.into_iter()
                        .map(|s| match s {
                            Shape::Number(n) => Shape::Number(n.map(|n| -n)),
                            Shape::TVar(t) => {
                                let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                let current_shape =
                                    Shape::merge_shapes(current_shape, Shape::Number(None), ctx);
                                ctx.shapes.insert(t, current_shape);

                                Shape::Number(None)
                            }
                            s => Shape::Mismatch(Box::new(s), Box::new(Shape::Number(None))),
                        })
                        .collect()
                }
            },
            Filter::BinOp(l, op, r) => match op {
                BinOp::Add => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);
                    tracing::debug!("computing {l} + {r}");
                    tracing::trace!("s1: {s1:?}");
                    tracing::trace!("s2: {s2:?}");

                    let result = s1
                        .into_iter()
                        .zip(s2)
                        .map(|(l, r)| {
                            match (l, r) {
                                // null + X
                                // X + null
                                (Shape::Null, s) | (s, Shape::Null) => s,
                                // str + str
                                (Shape::String(s1), Shape::String(s2)) => match (s1, s2) {
                                    (None, None) => Shape::String(None),
                                    (Some(s1), None) | (None, Some(s1)) => Shape::String(Some(s1)),
                                    (Some(s1), Some(s2)) => {
                                        Shape::String(Some(format!("{s1}{s2}")))
                                    }
                                },
                                (Shape::String(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::String(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Union(
                                            Box::new(Shape::String(None)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::String(None)
                                }
                                // arr + arr
                                (Shape::Array(shape1, u1), Shape::Array(shape2, u2)) => {
                                    let shape = Shape::merge_shapes(*shape1, *shape2, ctx);
                                    let u = u1.max(u2);

                                    Shape::Array(Box::new(shape), u)
                                }
                                (Shape::Array(shape, u), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Array(shape, u)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Union(
                                            Box::new(Shape::Array(shape.clone(), u)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::Array(shape, u)
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

                                    Shape::Object(obj.into_iter().collect())
                                }
                                (Shape::Object(obj), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Object(obj)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Object(obj.clone())),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::Object(obj)
                                }
                                // num + num
                                (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                                    (None, None) => Shape::Number(None),
                                    (Some(_), None) | (None, Some(_)) => Shape::Number(None),
                                    (Some(n1), Some(n2)) => Shape::Number(Some(n1 + n2)),
                                },
                                (Shape::Number(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Number(None)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::Number(None)
                                }
                                (l, r) => Shape::Mismatch(Box::new(l), Box::new(r)),
                            }
                        })
                        .collect();
                    tracing::debug!("result: {result:?}");
                    result
                }
                BinOp::Sub => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);
                    s1.into_iter()
                        .zip(s2)
                        .map(|(l, r)| {
                            tracing::debug!("computing {l} - {r}");
                            match (l, r) {
                                // arr - arr
                                (Shape::Array(shape1, u1), Shape::Array(shape2, u2)) => {
                                    let shape = Shape::merge_shapes(*shape1, *shape2, ctx);
                                    let u = u1.max(u2);

                                    Shape::Array(Box::new(shape), u)
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

                                    Shape::Array(shape, u)
                                }
                                // num - num
                                (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                                    (None, None) => Shape::Number(None),
                                    (Some(n1), None) | (None, Some(n1)) => Shape::Number(Some(n1)),
                                    (Some(n1), Some(n2)) => Shape::Number(Some(n1 - n2)),
                                },
                                (Shape::Number(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Number(None),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::Number(None)
                                }
                                (l, r) => {
                                    tracing::debug!("mismatch: {l} - {r}");
                                    Shape::Mismatch(Box::new(l), Box::new(r))
                                }
                            }
                        })
                        .collect()
                }
                BinOp::Mul => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);

                    s1.into_iter()
                        .zip(s2)
                        .map(|(l, r)| {
                            match (l, r) {
                                // num */% num
                                (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                                    (None, None) => Shape::Number(None),
                                    (Some(_), None) | (None, Some(_)) => Shape::Number(None),
                                    (Some(n1), Some(n2)) => Shape::Number(Some(n1 * n2)),
                                },
                                (Shape::Number(n), Shape::String(s))
                                | (Shape::String(s), Shape::Number(n)) => match (n, s) {
                                    (None, None) => Shape::Union(
                                        Box::new(Shape::String(None)),
                                        Box::new(Shape::Number(None)),
                                    ),
                                    (Some(_), None) | (None, Some(_)) => Shape::String(None),
                                    (Some(n), Some(s)) => Shape::String(Some(s.repeat(n as usize))),
                                },
                                (Shape::Number(n), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(n)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Number(None)),
                                            Box::new(Shape::String(None)),
                                        ),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    if let Some(n) = n {
                                        if n >= 0.0 {
                                            Shape::Union(
                                                Box::new(Shape::Number(None)),
                                                Box::new(Shape::String(None)),
                                            )
                                        } else {
                                            Shape::Union(
                                                Box::new(Shape::Number(None)),
                                                Box::new(Shape::Null),
                                            )
                                        }
                                    } else {
                                        Shape::Union(
                                            Box::new(Shape::Union(
                                                Box::new(Shape::Number(None)),
                                                Box::new(Shape::String(None)),
                                            )),
                                            Box::new(Shape::Null),
                                        )
                                    }
                                }
                                (l, r) => Shape::Mismatch(Box::new(l), Box::new(r)),
                            }
                        })
                        .collect()
                }
                BinOp::Div | BinOp::Mod => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);

                    s1.into_iter()
                        .zip(s2)
                        .map(|(l, r)| {
                            match (l, r) {
                                // num */% num
                                (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                                    (None, None) => Shape::Number(None),
                                    (Some(n1), None) | (None, Some(n1)) => Shape::Number(Some(n1)),
                                    (Some(n1), Some(n2)) => match op {
                                        BinOp::Div => Shape::Number(Some(n1 / n2)),
                                        BinOp::Mod => Shape::Number(Some(n1 % n2)),
                                        _ => unreachable!(),
                                    },
                                },
                                (Shape::Number(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(_)) => {
                                    let current_shape = ctx.shapes.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Number(None),
                                        ctx,
                                    );
                                    ctx.shapes.insert(t, current_shape);

                                    Shape::Number(None)
                                }
                                (l, r) => Shape::Mismatch(Box::new(l), Box::new(r)),
                            }
                        })
                        .collect()
                }
                BinOp::Gt
                | BinOp::Ge
                | BinOp::Lt
                | BinOp::Le
                | BinOp::And
                | BinOp::Or
                | BinOp::Eq
                | BinOp::Ne => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);

                    s1.into_iter()
                        .zip(s2)
                        .map(|(l, r)| match (l, r) {
                            (Shape::Mismatch(l, r), _) | (_, Shape::Mismatch(l, r)) => {
                                Shape::Mismatch(l, r)
                            }
                            _ => Shape::Bool(None),
                        })
                        .collect()
                }
            },
            Filter::Empty => vec![],
            Filter::Error => todo!(),
            Filter::Call(name, filters_) => match filters_ {
                Some(args) => {
                    let filter = filters
                        .get(name)
                        .expect(format!("filter {name} not found").as_str());
                    if let Filter::Bound(vars, filter) = filter {
                        let mut filter = *filter.clone();
                        for (var, arg) in vars.iter().zip(args.iter()) {
                            filter = filter.substitute(var, arg);
                        }
                        Shape::build_shape(&filter, shapes, ctx, filters)
                    } else {
                        panic!("expected bound filter, found '{}'", filter);
                    }
                    // Shape::build_shape(filter, shapes, ctx, filters)
                }
                None => {
                    let filter = filters.get(name).unwrap_or(&Filter::Dot);
                    Shape::build_shape(filter, shapes, ctx, filters)
                }
            },
            Filter::IfThenElse(filter, filter1, filter2) => {
                let s = Shape::build_shape(filter, shapes.clone(), ctx, filters);
                let s1 = Shape::build_shape(filter1, shapes.clone(), ctx, filters);
                let s2 = Shape::build_shape(filter2, shapes, ctx, filters);

                s.into_iter()
                    .zip(s1)
                    .zip(s2)
                    .map(|((s, s1), s2)| match (s, s1, s2) {
                        (Shape::Bool(Some(true)), s1, _) => s1,
                        (Shape::Bool(Some(false)), _, s2) => s2,
                        (Shape::Bool(None), s1, s2) => Shape::Union(Box::new(s1), Box::new(s2)),
                        (s, _, _) => Shape::Mismatch(Box::new(s), Box::new(Shape::Bool(None))),
                    })
                    .collect()
            }
            Filter::Bound(items, filter) => {
                // todo: understand this better
                Shape::build_shape(filter, shapes, ctx, filters)
            }
            Filter::FunctionExpression(_, expr) => todo!(),
        }
    }

    pub fn merge_shapes(s1: Shape, s2: Shape, ctx: &mut ShapeContext) -> Shape {
        match (s1, s2) {
            (Shape::Blob, s) | (s, Shape::Blob) => s,
            (Shape::TVar(t), s) | (s, Shape::TVar(t)) => {
                let current_shape = ctx.shapes.get(&t).unwrap().clone();
                match current_shape {
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
                }
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
                    // todo: this produces a null, should we add a null strictness level?
                    //  if let Some(u) = u {
                    //     if *u > arr.len() {
                    //         return Some(ShapeMismatch::new(
                    //             path,
                    //             self.clone(),
                    //             Shape::Tuple(arr.into_iter().map(Shape::from_json).collect()),
                    //         ));
                    //     }
                    // }

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
            Shape::Union(s1, s2) => {
                let (matches, mismatches): (Vec<_>, Vec<_>) = vec![s1, s2]
                    .iter()
                    .map(|s| Shape::check(s, j.clone(), path.clone()))
                    .partition(Option::is_none);

                if !matches.is_empty() {
                    None
                } else {
                    let mut mismatch = mismatches
                        .into_iter()
                        .next()
                        .flatten()
                        .expect("expect 1 mismatch");
                    mismatch.expected = self.clone();
                    Some(mismatch)
                }
            }
        }
    }

    pub fn check_self(&self, path: Vec<Access>) -> Option<ShapeMismatch> {
        println!("checking self: {self}");
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
            Shape::Union(s1, s2) => {
                let (_, mismatches): (Vec<_>, Vec<_>) = vec![s1, s2]
                    .iter()
                    .map(|s| s.check_self(path.clone()))
                    .partition(Option::is_none);

                mismatches.into_iter().next().flatten()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::shape::{Access, Shape, ShapeMismatch};
    use std::collections::HashMap;
    use tjq_exec::{BinOp, Filter, Json, UnOp};

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

        let mut filters = HashMap::new();
        filters.insert("map".to_string(), map);
        filters.insert("abs".to_string(), abs);
        filters.insert("isboolean".to_string(), isboolean);
        filters.insert("type".to_string(), type_);

        filters
    }
    
    #[test]
    fn test_plus1() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Add,
            Box::new(Filter::Dot),
        );

        let (shape, results) = Shape::new(&filter, &HashMap::new());

        assert_eq!(
            shape,
            Shape::Union(Box::new(Shape::Number(None)), Box::new(Shape::Null))
        );
        assert_eq!(results.len(), 1);
        assert_eq!(results.first().unwrap(), &Shape::Number(None));

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    #[test]
    fn test_plus2() {
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

        let (shape, results) = Shape::new(&filter, &HashMap::new());

        assert_eq!(
            shape,
            Shape::Mismatch(
                Box::new(Shape::Union(
                    Box::new(Shape::Number(None)),
                    Box::new(Shape::Null)
                )),
                Box::new(Shape::Union(
                    Box::new(Shape::String(None)),
                    Box::new(Shape::Null)
                )),
            )
        );

        assert_eq!(results.len(), 2);

        assert_eq!(results.first().unwrap(), &Shape::Number(None));

        assert_eq!(results.last().unwrap(), &Shape::String(None));
    }

    #[test]
    fn test_minus1() {
        let json = Json::Number(1.0);
        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Sub,
            Box::new(Filter::Number(2.0)),
        );

        let (shape, results) = Shape::new(&filter, &HashMap::new());

        assert_eq!(shape, Shape::Blob);
        assert_eq!(results.len(), 1);
        assert_eq!(results.first().unwrap(), &Shape::Number(Some(-1.0)));

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    #[test]
    fn test_minus2() {
        let json1 = Json::Number(1.0);
        let json2 = Json::Object(vec![("a".to_string(), Json::Number(2.0))]);

        let filter = Filter::BinOp(
            Box::new(Filter::Number(1.0)),
            BinOp::Sub,
            Box::new(Filter::ObjIndex("a".to_string())),
        );

        let (shape, results) = Shape::new(&filter, &HashMap::new());

        assert_eq!(
            shape,
            Shape::Object(vec![("a".to_string(), Shape::Number(None))])
        );

        assert_eq!(results, vec![Shape::Number(None)]);

        let results = shape.check(json1, vec![]);

        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![],
                Shape::Object(vec![("a".to_string(), Shape::Number(None))]),
                Shape::Number(Some(1.0)),
            ))
        );

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_minus3() {
        // "x" - .a
        let filter = Filter::BinOp(
            Box::new(Filter::String("x".to_string())),
            BinOp::Sub,
            Box::new(Filter::ObjIndex("a".to_string())),
        );

        let (shape, results) = Shape::new(&filter, &HashMap::new());

        assert_eq!(shape, Shape::Object(vec![("a".to_string(), Shape::Blob)]));

        assert_eq!(results.len(), 1);
        assert_eq!(
            results.first().unwrap(),
            &Shape::Mismatch(
                Box::new(Shape::String(Some("x".to_string()))),
                Box::new(Shape::Blob)
            )
        );
    }

    #[test]
    fn test_abs() {
        let filter = Filter::Call("abs".to_string(), None);

        let (shape, results) = Shape::new(&filter, &builtin_filters());
        assert_eq!(shape, Shape::Number(None));

        assert_eq!(results.len(), 1);
        assert_eq!(results.first().unwrap().canonicalize(), Shape::Number(None));
    }

    #[test]
    fn test_isboolean() {
        let json1 = Json::Boolean(true);
        let json2 = Json::Number(1.0);
        let filter = Filter::Call("isboolean".to_string(), None);

        let (shape, results) = Shape::new(&filter, &builtin_filters());
        assert_eq!(shape, Shape::Blob);
        assert_eq!(results, vec![Shape::Bool(None)]);

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_type() {
        let json1 = Json::Boolean(true);
        let json2 = Json::Number(1.0);

        let filter = Filter::Call("type".to_string(), None);

        let (shape, results) = Shape::new(&filter, &builtin_filters());

        assert_eq!(shape, Shape::Blob);

        assert_eq!(
            results,
            vec![Shape::Union(
                Box::new(Shape::String(Some("null".to_string()))),
                Box::new(Shape::Union(
                    Box::new(Shape::String(Some("boolean".to_string()))),
                    Box::new(Shape::Union(
                        Box::new(Shape::String(Some("number".to_string()))),
                        Box::new(Shape::Union(
                            Box::new(Shape::String(Some("string".to_string()))),
                            Box::new(Shape::Union(
                                Box::new(Shape::String(Some("array".to_string()))),
                                Box::new(Shape::String(Some("object".to_string())))
                            ))
                        ))
                    ))
                ))
            )]
        );

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_map1() {
        let json1 = Json::Array(vec![Json::Number(-1.0), Json::Number(2.0)]);
        let json2 = Json::Array(vec![
            Json::String("hello".to_string()),
            Json::String("world".to_string()),
        ]);
        let json3 = Json::Array(vec![Json::Number(-1.0), Json::String("world".to_string())]);
        let json4 = Json::Array(vec![Json::String("hello".to_string()), Json::Boolean(true)]);

        // map(. * 2)
        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Mul,
                Box::new(Filter::Number(2.0)),
            )]),
        );

        let (shape, results) = Shape::new(&filter, &builtin_filters());
        assert_eq!(
            shape,
            Shape::Array(
                Box::new(Shape::Union(
                    Box::new(Shape::Number(None)),
                    Box::new(Shape::String(None))
                )),
                None
            )
        );

        assert_eq!(results.len(), 1);
        assert_eq!(
            results.first().unwrap(),
            &Shape::Array(
                Box::new(Shape::Union(
                    Box::new(Shape::Number(None)),
                    Box::new(Shape::String(None))
                )),
                None
            )
        );

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json3, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json4, vec![]);

        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![Access::Array(1)],
                Shape::Union(Box::new(Shape::Number(None)), Box::new(Shape::String(None))),
                Shape::Bool(Some(true))
            ))
        );
    }

    #[test]
    fn test_map2() {
        let json1 = Json::Array(vec![Json::Number(-1.0), Json::Number(2.0)]);
        let json2 = Json::Array(vec![
            Json::String("hello".to_string()),
            Json::String("world".to_string()),
        ]);
        let json3 = Json::Array(vec![Json::Number(-1.0), Json::String("world".to_string())]);
        let json4 = Json::Array(vec![Json::Null, Json::Boolean(true)]);

        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::Bound(
                vec![],
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Add,
                    Box::new(Filter::Number(2.0)),
                )),
            )]),
        );

        let (shape, results) = Shape::new(&filter, &builtin_filters());

        assert_eq!(
            shape,
            Shape::Array(
                Box::new(Shape::Union(
                    Box::new(Shape::Number(None)),
                    Box::new(Shape::Null)
                )),
                None
            )
        );

        assert_eq!(results.len(), 1);
        assert_eq!(
            results.first().unwrap(),
            &Shape::Array(Box::new(Shape::Number(None)), None)
        );

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![Access::Array(0)],
                Shape::Union(Box::new(Shape::Number(None)), Box::new(Shape::Null)),
                Shape::String(Some("hello".to_string()))
            ))
        );

        let results = shape.check(json3, vec![]);
        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![Access::Array(1)],
                Shape::Union(Box::new(Shape::Number(None)), Box::new(Shape::Null)),
                Shape::String(Some("world".to_string()))
            ))
        );

        let results = shape.check(json4, vec![]);
        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![Access::Array(1)],
                Shape::Union(Box::new(Shape::Number(None)), Box::new(Shape::Null)),
                Shape::Bool(Some(true))
            ))
        );
    }

    #[test]
    fn test_map3() {
        let json = Json::Array(vec![
            Json::Array(vec![Json::Number(-1.0), Json::Number(-2.0)]),
            Json::Array(vec![Json::Number(3.0), Json::Number(4.0)]),
        ]);
        let filter = Filter::Call(
            "map".to_string(),
            Some(vec![Filter::Call(
                "map".to_string(),
                Some(vec![Filter::Call("abs".to_string(), None)]),
            )]),
        );

        let (shape, results) = Shape::new(&filter, &builtin_filters());

        assert_eq!(
            shape,
            Shape::Array(
                Box::new(Shape::Array(Box::new(Shape::Number(None)), None)),
                None
            )
        );

        assert_eq!(results.len(), 1);

        assert_eq!(
            results.first().unwrap().canonicalize(),
            Shape::Array(
                Box::new(Shape::Array(Box::new(Shape::Number(None)), None)),
                None
            )
        );

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }
}
