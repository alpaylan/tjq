use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter, Json, UnOp};
use topological_sort::TopologicalSort;

use crate::inference::TypeInference;

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
    Object(Row),
    Mismatch(Box<Shape>, Box<Shape>),
    Union(Box<Shape>, Box<Shape>),
    Intersection(Box<Shape>, Box<Shape>),
    Neg(Box<Shape>),
    /// Arrow type for function/filter types: input -> output
    Arrow(Box<Shape>, Box<Shape>),
}

/// An object type as a row (docs/type-system-scope.md §6): a set of named
/// fields plus whether keys beyond them are permitted. A JSON object `o`
/// inhabits the row iff every field is satisfied (a required field must be
/// present with a conforming value; an optional field, if present, must
/// conform) and, when the row is closed, `o` has no keys outside the row.
#[derive(Debug, Clone, PartialEq)]
pub struct Row {
    pub fields: Vec<Field>,
    /// Extra keys (beyond `fields`) are permitted, each with any value.
    /// Field *access* patterns infer open rows; object *construction*
    /// infers closed ones.
    pub open: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub key: String,
    pub value: Shape,
    /// The key may be absent from a conforming object (jq's lenient `.a`,
    /// which yields null on a missing key, demands an optional field).
    pub optional: bool,
}

impl Row {
    pub fn iter(&self) -> std::slice::Iter<'_, Field> {
        self.fields.iter()
    }
    pub fn get(&self, key: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.key == key)
    }
    pub fn len(&self) -> usize {
        self.fields.len()
    }
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

impl std::ops::Index<usize> for Row {
    type Output = Field;
    fn index(&self, i: usize) -> &Field {
        &self.fields[i]
    }
}

impl IntoIterator for Row {
    type Item = Field;
    type IntoIter = std::vec::IntoIter<Field>;
    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

impl<'a> IntoIterator for &'a Row {
    type Item = &'a Field;
    type IntoIter = std::slice::Iter<'a, Field>;
    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter()
    }
}

impl Shape {
    pub(crate) fn null() -> Self {
        Shape::Null
    }
    pub(crate) fn tvar(t: usize) -> Self {
        Shape::TVar(t)
    }
    pub(crate) fn bool(b: bool) -> Self {
        Shape::Bool(Some(b))
    }
    pub(crate) fn bool_() -> Self {
        Shape::Bool(None)
    }

    pub(crate) fn number(n: impl Into<f64>) -> Self {
        Shape::Number(Some(n.into()))
    }
    pub(crate) fn number_() -> Self {
        Shape::Number(None)
    }

    pub(crate) fn string(s: impl Into<String>) -> Self {
        Shape::String(Some(s.into()))
    }
    pub(crate) fn string_() -> Self {
        Shape::String(None)
    }

    pub(crate) fn blob() -> Self {
        Shape::Blob
    }
    pub(crate) fn array(shape: Shape, u: Option<isize>) -> Self {
        Shape::Array(Box::new(shape), u)
    }
    pub(crate) fn tuple(shapes: Vec<Shape>) -> Self {
        Shape::Tuple(shapes)
    }
    /// Back-compat constructor: an OPEN row of all-REQUIRED fields — exactly
    /// the semantics `Object(Vec<(String, Shape)>)` had before rows. Most
    /// call sites (field-access demands, `from_json`) want this.
    pub(crate) fn object(items: Vec<(String, Shape)>) -> Self {
        Shape::Object(Row {
            fields: items
                .into_iter()
                .map(|(key, value)| Field {
                    key,
                    value,
                    optional: false,
                })
                .collect(),
            open: true,
        })
    }

    /// A CLOSED row of all-required fields: the object has exactly these
    /// keys. Object construction (`{a: 1}`) produces this.
    pub(crate) fn object_closed(items: Vec<(String, Shape)>) -> Self {
        Shape::Object(Row {
            fields: items
                .into_iter()
                .map(|(key, value)| Field {
                    key,
                    value,
                    optional: false,
                })
                .collect(),
            open: false,
        })
    }
    pub(crate) fn mismatch(s1: Shape, s2: Shape) -> Self {
        Shape::Mismatch(Box::new(s1), Box::new(s2))
    }
    pub(crate) fn union(s1: Shape, s2: Shape) -> Self {
        Shape::Union(Box::new(s1), Box::new(s2))
    }
    pub(crate) fn union_(shapes: Vec<Shape>) -> Self {
        if shapes.is_empty() {
            panic!("Cannot create union of empty shapes");
        }
        let mut iter = shapes.into_iter().rev();
        let mut result = iter.next().unwrap();
        for shape in iter {
            result = Shape::union(result, shape);
        }
        result
    }

    pub(crate) fn neg(self) -> Self {
        Shape::Neg(Box::new(self))
    }

    pub(crate) fn intersection(s1: Shape, s2: Shape) -> Self {
        Shape::Intersection(Box::new(s1), Box::new(s2))
    }

    pub(crate) fn intersection_(shapes: Vec<Shape>) -> Self {
        if shapes.is_empty() {
            panic!("Cannot create intersection of empty shapes");
        }
        shapes
            .into_iter()
            .reduce(|a, b| Shape::intersection(a, b))
            .unwrap()
    }

    pub(crate) fn arrow(input: Shape, output: Shape) -> Self {
        Shape::Arrow(Box::new(input), Box::new(output))
    }
}

impl PartialOrd for Shape {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Shape::TVar(_), _) => None,
            (_, Shape::TVar(_)) => None,
            (Shape::Blob, _) => None,
            (_, Shape::Blob) => None,
            // Anything is greater than a NULL
            (Shape::Null, Shape::Null) => Some(Ordering::Equal),
            (Shape::Null, _) => Some(Ordering::Less),
            (_, Shape::Null) => Some(Ordering::Greater),
            // Anything !(NULL) is greater than Bool
            (Shape::Bool(Some(b1)), Shape::Bool(Some(b2))) => Some(b1.cmp(b2)), // Concrete bool check
            (Shape::Bool(None), Shape::Bool(Some(_)))
            | (Shape::Bool(Some(_)), Shape::Bool(None)) => None, // Unknown bool check
            (Shape::Bool(_), _) => Some(Ordering::Less),
            (_, Shape::Bool(_)) => Some(Ordering::Greater),
            // Anything !(NULL | Bool) is greater than Number
            (Shape::Number(Some(n1)), Shape::Number(Some(n2))) => n1.partial_cmp(n2), // Concrete number check
            (Shape::Number(None), Shape::Number(Some(_)))
            | (Shape::Number(Some(_)), Shape::Number(None)) => None, // Unknown number check
            (Shape::Number(_), _) => Some(Ordering::Less),
            (_, Shape::Number(_)) => Some(Ordering::Greater),
            // Anything !(NULL | Bool | Number) is greater than String
            (Shape::String(Some(s1)), Shape::String(Some(s2))) => Some(s1.cmp(s2)),
            (Shape::String(None), Shape::String(Some(_)))
            | (Shape::String(Some(_)), Shape::String(None)) => None, // Unknown string check
            (Shape::String(_), _) => Some(Ordering::Less),
            (_, Shape::String(_)) => Some(Ordering::Greater),
            // Anything !(NULL | Bool | Number | String) is greater than Array
            (Shape::Array(_, _), Shape::Array(_, _)) => None, // Arrays are incomparable
            (Shape::Tuple(shapes), Shape::Tuple(shapes2)) => {
                // Compare tuple shapes lexicographically
                shapes
                    .iter()
                    .zip(shapes2.iter())
                    .find(|(s1, s2)| {
                        s1.partial_cmp(s2)
                            .map(|o| o != Ordering::Equal)
                            .unwrap_or(false)
                    })
                    .map(|(s1, s2)| s1.partial_cmp(s2).unwrap())
            }
            (Shape::Array(_, _), Shape::Tuple(_)) | (Shape::Tuple(_), Shape::Array(_, _)) => None, // Arrays are incomparable with tuples
            (Shape::Array(_, _), _) | (Shape::Tuple(_), _) => Some(Ordering::Less),
            (_, Shape::Array(_, _)) | (_, Shape::Tuple(_)) => Some(Ordering::Greater),
            // Objects are greater than all
            (Shape::Object(_), Shape::Object(_)) => None, // Objects are incomparable
            (_, Shape::Object(_)) => Some(Ordering::Less),
            (Shape::Object(_), _) => Some(Ordering::Greater),
            _ => None,
        }
    }
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
            Shape::TVar(t) => write!(f, "<T{t}>",),
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
                for (i, field) in obj.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    let opt = if field.optional { "?" } else { "" };
                    write!(f, "{}{}: {}", field.key, opt, field.value)?;
                }
                if obj.open {
                    if obj.fields.is_empty() {
                        write!(f, "..")?;
                    } else {
                        write!(f, ", ..")?;
                    }
                }
                write!(f, "}}")
            }
            Shape::Mismatch(s1, s2) => write!(f, "({s1} >< {s2})"),
            Shape::Union(s1, s2) => write!(f, "({s1} | {s2})"),
            Shape::Neg(shape) => {
                write!(f, "(!{shape})")
            }
            Shape::Intersection(s1, s2) => write!(f, "({s1} & {s2})"),
            Shape::Arrow(input, output) => write!(f, "({input} -> {output})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ShapeContext(HashMap<usize, Shape>);

impl ShapeContext {
    fn new() -> Self {
        ShapeContext(HashMap::new())
    }

    fn fresh(&self) -> usize {
        self.0.len()
    }

    fn get(&self, t: &usize) -> Option<&Shape> {
        self.0.get(t)
    }

    fn get_mut(&mut self, t: &usize) -> Option<&mut Shape> {
        self.0.get_mut(t)
    }

    fn insert(&mut self, t: usize, shape: Shape) {
        self.0.insert(t, shape);
    }

    fn remove(&mut self, t: &usize) -> Option<Shape> {
        self.0.remove(t)
    }

    fn normalize(&mut self) {
        // 1. topological sort
        // 2. turn all self referential variables into blobs
        // 3. turn all non self referential variables into their actual shape

        // self.shapes.iter_mut().for_each(|(k, v)| {
        //     if let Shape::TVar(t) = v {
        //         tracing::debug!("Checking {t} == {k}");
        //         if t == k {
        //             *v = Shape::Blob;
        //         }
        //     }
        // });

        let shapes = self.0.clone();

        let dependencies = shapes
            .iter()
            .map(|(k, v)| (*k, v.dependencies()))
            .collect::<HashMap<_, _>>();

        let mut sorted = toposort(dependencies);

        while let Some(t) = sorted.pop() {
            let shape = self.get(&t).unwrap().clone();
            if let Shape::TVar(t_) = shape {
                tracing::debug!("Normalizing {t} -> {t_}");
                if t == t_ {
                    self.insert(t, Shape::Blob);
                } else {
                    let shape = self.get(&t_).unwrap().clone();
                    self.insert(t, shape);
                }
            } else {
                let shape = shape.normalize(self);
                self.insert(t, shape);
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

impl Display for Subtyping {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Subtyping::Subtype => write!(f, "<:"),
            Subtyping::Supertype => write!(f, ":>"),
            Subtyping::Incompatible => write!(f, "<:>"),
        }
    }
}

/// Direct type inference algorithm.
/// Uses the original build_shape approach for type inference.
pub struct DirectInference;

impl TypeInference for DirectInference {
    fn infer(&self, f: &Filter, filters: &HashMap<String, Filter>) -> (Shape, Vec<Shape>) {
        let mut ctx = ShapeContext::new();
        ctx.insert(0, Shape::tvar(0));

        let results = Shape::build_shape(f, vec![Shape::tvar(0)], &mut ctx, filters);
        tracing::debug!("type context: {:?}", ctx);
        tracing::debug!("result types: {:?}", results);
        ctx.normalize();

        (ctx.remove(&0).unwrap(), results)
    }
}

impl Shape {
    /// Number of nodes in the shape tree. Used to bound substitution
    /// growth: cyclic equalities with several occurrences of the same
    /// variable double the shape every substitution round.
    pub fn size(&self) -> usize {
        1 + match self {
            Shape::TVar(_)
            | Shape::Blob
            | Shape::Null
            | Shape::Bool(_)
            | Shape::Number(_)
            | Shape::String(_) => 0,
            Shape::Array(e, _) => e.size(),
            Shape::Tuple(ts) => ts.iter().map(|s| s.size()).sum(),
            Shape::Object(fs) => fs.iter().map(|f| f.value.size()).sum(),
            Shape::Mismatch(a, b)
            | Shape::Union(a, b)
            | Shape::Intersection(a, b)
            | Shape::Arrow(a, b) => a.size() + b.size(),
            Shape::Neg(a) => a.size(),
        }
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
            Shape::Object(obj) => obj.iter().flat_map(|f| f.value.dependencies()).collect(),
            Shape::Mismatch(s1, s2)
            | Shape::Union(s1, s2)
            | Shape::Intersection(s1, s2)
            | Shape::Arrow(s1, s2) => s1
                .dependencies()
                .into_iter()
                .chain(s2.dependencies())
                .collect(),
            Shape::Neg(shape) => shape.dependencies(),
        }
    }

    pub fn normalize(&self, ctx: &mut ShapeContext) -> Shape {
        tracing::debug!("normalizing {self}");
        match self {
            Shape::TVar(t) => {
                let shape = ctx.get(t).unwrap().clone();
                if let Shape::TVar(t_) = &shape {
                    if t == t_ {
                        Shape::Blob
                    } else {
                        shape.normalize(ctx)
                    }
                } else {
                    shape.normalize(ctx)
                }
            }
            Shape::Blob => Shape::Blob,
            Shape::Null => Shape::Null,
            Shape::Bool(b) => Shape::Bool(*b),
            Shape::Number(n) => Shape::Number(*n),
            Shape::String(s) => Shape::String(s.clone()),
            Shape::Array(shape, u) => Shape::Array(Box::new(shape.normalize(ctx)), *u),
            Shape::Tuple(tuple) => Shape::Tuple(tuple.iter().map(|s| s.normalize(ctx)).collect()),
            Shape::Object(obj) => Shape::Object(Row {
                fields: obj
                    .iter()
                    .map(|f| Field {
                        key: f.key.clone(),
                        value: f.value.normalize(ctx),
                        optional: f.optional,
                    })
                    .collect(),
                open: obj.open,
            }),
            Shape::Mismatch(s1, s2) => {
                Shape::Mismatch(Box::new(s1.normalize(ctx)), Box::new(s2.normalize(ctx)))
            }
            Shape::Union(s1, s2) => {
                Shape::Union(Box::new(s1.normalize(ctx)), Box::new(s2.normalize(ctx)))
            }
            Shape::Neg(shape) => Shape::Neg(Box::new(shape.normalize(ctx))),
            Shape::Intersection(s1, s2) => {
                Shape::Intersection(Box::new(s1.normalize(ctx)), Box::new(s2.normalize(ctx)))
            }
            Shape::Arrow(input, output) => Shape::Arrow(
                Box::new(input.normalize(ctx)),
                Box::new(output.normalize(ctx)),
            ),
        }
    }

    /// Semantic inclusion: is every JSON value accepted by `self` also
    /// accepted by `other`? (Standard subtyping, `self ⊆ other`, with
    /// `check` as the denotation.)
    ///
    /// Conservative: `true` is a proof obligation (validated by the model
    /// tests in tjq_testing), `false` means "not provably included". This
    /// is the replacement direction for the legacy tri-state `subtype`,
    /// which uses an inverted convention (see its doc comment).
    pub fn included_in(&self, other: &Shape) -> bool {
        match (self, other) {
            // Top and bottom
            (_, Shape::Blob) => true,
            (Shape::Mismatch(_, _), _) => true, // empty set is included in everything
            // Unknowns: no claims
            (Shape::TVar(_), _) | (_, Shape::TVar(_)) => false,
            // Set operators on the left
            (Shape::Union(a, b), t) => a.included_in(t) && b.included_in(t),
            (Shape::Intersection(a, b), t) => a.included_in(t) || b.included_in(t),
            // Set operators on the right
            (t, Shape::Intersection(a, b)) => t.included_in(a) && t.included_in(b),
            (t, Shape::Union(a, b)) => t.included_in(a) || t.included_in(b),
            (Shape::Neg(a), Shape::Neg(b)) => b.included_in(a),
            (t, Shape::Neg(b)) => t.disjoint_with(b),
            (Shape::Neg(_), _) => false, // ¬A ⊆ B needs completeness reasoning; stay conservative
            // Scalars
            (Shape::Null, Shape::Null) => true,
            (Shape::Bool(_), Shape::Bool(None)) => true,
            (Shape::Bool(Some(a)), Shape::Bool(Some(b))) => a == b,
            (Shape::Number(_), Shape::Number(None)) => true,
            (Shape::Number(Some(a)), Shape::Number(Some(b))) => a == b,
            (Shape::String(_), Shape::String(None)) => true,
            (Shape::String(Some(a)), Shape::String(Some(b))) => a == b,
            // Arrays are covariant in the element type; `check` ignores the
            // length annotation, so inclusion does too.
            (Shape::Array(e1, _), Shape::Array(e2, _)) => e1.included_in(e2),
            // A tuple accepts arrays with at least its prefix, rest
            // unconstrained; it fits in Array(e) only if e is ⊤.
            (Shape::Tuple(ts), Shape::Array(e, _)) => {
                matches!(e.as_ref(), Shape::Blob) && ts.iter().all(|t| t.included_in(e))
            }
            // Array(e) contains the empty array, so it only fits in an
            // empty-prefix tuple (which accepts every array).
            (Shape::Array(_, _), Shape::Tuple(ts)) => ts.is_empty(),
            // Tuple ⊆ Tuple: at least as long a prefix, pointwise included
            (Shape::Tuple(t1), Shape::Tuple(t2)) => {
                t1.len() >= t2.len() && t2.iter().zip(t1).all(|(s2, s1)| s1.included_in(s2))
            }
            // Row subtyping: every inhabitant of o1 must inhabit o2.
            (Shape::Object(o1), Shape::Object(o2)) => {
                // Each field o2 constrains must hold for every o1-inhabitant.
                let fields_ok = o2.iter().all(|f2| match o1.get(&f2.key) {
                    Some(f1) => {
                        // Present-value bound must refine; and if o1 may omit
                        // the key, o2 must tolerate the absence too.
                        f1.value.included_in(&f2.value) && (!f1.optional || f2.optional)
                    }
                    None if o1.open => {
                        // o1 leaves the key unconstrained (any value or
                        // absent); o2 can only cover that if it accepts
                        // anything and allows absence.
                        matches!(f2.value, Shape::Blob) && f2.optional
                    }
                    // closed o1 never carries the key, so o2 must allow absence
                    None => f2.optional,
                });
                // A closed o2 forbids keys o1 might carry beyond o2's fields.
                let closed_ok =
                    o2.open || (!o1.open && o1.iter().all(|f1| o2.get(&f1.key).is_some()));
                fields_ok && closed_ok
            }
            _ => false,
        }
    }

    /// Provable disjointness: no JSON value inhabits both shapes.
    /// Conservative: `false` means "not provably disjoint".
    pub fn disjoint_with(&self, other: &Shape) -> bool {
        use Shape::*;
        let kind_disjoint = |a: &Shape, b: &Shape| -> bool {
            // Kind classes: null / bool / number / string / array-like / object
            fn kind(s: &Shape) -> Option<u8> {
                match s {
                    Null => Some(0),
                    Bool(_) => Some(1),
                    Number(_) => Some(2),
                    String(_) => Some(3),
                    Array(_, _) | Tuple(_) => Some(4),
                    Object(_) => Some(5),
                    _ => None,
                }
            }
            match (kind(a), kind(b)) {
                (Some(ka), Some(kb)) => ka != kb,
                _ => false,
            }
        };
        match (self, other) {
            (Mismatch(_, _), _) | (_, Mismatch(_, _)) => true,
            (TVar(_), _) | (_, TVar(_)) => false,
            (Blob, _) | (_, Blob) => false,
            (Union(a, b), t) | (t, Union(a, b)) => t.disjoint_with(a) && t.disjoint_with(b),
            (Intersection(a, b), t) | (t, Intersection(a, b)) => {
                t.disjoint_with(a) || t.disjoint_with(b)
            }
            (Neg(a), t) | (t, Neg(a)) => t.included_in(a),
            (Bool(Some(a)), Bool(Some(b))) => a != b,
            (Number(Some(a)), Number(Some(b))) => a != b,
            (String(Some(a)), String(Some(b))) => a != b,
            (a, b) => kind_disjoint(a, b),
        }
    }

    /// Legacy tri-state subtyping used by the constraint solver.
    ///
    /// NOTE: the convention is *inverted* relative to standard subtyping:
    /// `Subtyping::Subtype` means `self ⊇ other` (self is the more general
    /// type, or equal), and `Subtyping::Supertype` means `self ⊂ other`
    /// (self is strictly more specific). `Incompatible` makes no claim.
    /// New code should use `included_in` / `disjoint_with`; the model tests
    /// in tjq_testing enforce both contracts.
    pub fn subtype(&self, other: &Self) -> Subtyping {
        // Derived from the model-tested semantic primitives so the claims
        // are sound by construction; Incompatible is returned whenever
        // inclusion is not provable in either direction.
        if other.included_in(self) {
            Subtyping::Subtype
        } else if self.included_in(other) {
            Subtyping::Supertype
        } else {
            Subtyping::Incompatible
        }
    }

    pub fn canonicalize(&self) -> Shape {
        match self {
            Shape::TVar(t) => Shape::TVar(*t),
            Shape::Blob | Shape::Null | Shape::Bool(_) | Shape::Number(_) | Shape::String(_) => {
                self.clone()
            }
            Shape::Array(shape, u) => Shape::Array(Box::new(shape.canonicalize()), *u),
            Shape::Tuple(shapes) => {
                Shape::Tuple(shapes.iter().map(|s| s.canonicalize()).collect::<Vec<_>>())
            }
            Shape::Object(items) => Shape::Object(Row {
                fields: items
                    .iter()
                    .map(|f| Field {
                        key: f.key.clone(),
                        value: f.value.canonicalize(),
                        optional: f.optional,
                    })
                    .collect(),
                open: items.open,
            }),
            Shape::Mismatch(s1, s2) => {
                Shape::Mismatch(Box::new(s1.canonicalize()), Box::new(s2.canonicalize()))
            }
            Shape::Union(s1, s2) => {
                // Absorption via semantic inclusion: A | B = B when A ⊆ B.
                let s1 = s1.canonicalize();
                let s2 = s2.canonicalize();
                if s1.included_in(&s2) {
                    s2
                } else if s2.included_in(&s1) {
                    s1
                } else if matches!(
                    (&s1, &s2),
                    (Shape::Bool(Some(true)), Shape::Bool(Some(false)))
                        | (Shape::Bool(Some(false)), Shape::Bool(Some(true)))
                ) {
                    // Exact promotion: true | false covers all of bool
                    Shape::Bool(None)
                } else {
                    Shape::Union(Box::new(s1), Box::new(s2))
                }
            }
            Shape::Neg(shape) => match *shape.clone() {
                // Basic types: canonicalize inner and keep Neg wrapper
                Shape::TVar(_)
                | Shape::Blob
                | Shape::Null
                | Shape::Bool(_)
                | Shape::Number(_)
                | Shape::String(_)
                | Shape::Array(_, _)
                | Shape::Tuple(_)
                | Shape::Object(_)
                | Shape::Arrow(_, _) => Shape::Neg(Box::new(shape.canonicalize())),
                // Double negation cancels out
                Shape::Neg(inner) => inner.canonicalize(),
                // De Morgan's law: Neg(A | B) = Neg(A) & Neg(B)
                Shape::Union(s1, s2) => Shape::Intersection(
                    Box::new(Shape::Neg(s1).canonicalize()),
                    Box::new(Shape::Neg(s2).canonicalize()),
                )
                .canonicalize(),
                // Mismatch is the empty set; its complement is everything
                Shape::Mismatch(_, _) => Shape::Blob,
                // De Morgan's law: Neg(A & B) = Neg(A) | Neg(B)
                Shape::Intersection(s1, s2) => Shape::Union(
                    Box::new(Shape::Neg(s1).canonicalize()),
                    Box::new(Shape::Neg(s2).canonicalize()),
                ),
            },
            Shape::Intersection(s1, s2) => {
                let s1 = s1.canonicalize();
                let s2 = s2.canonicalize();

                // A & A = A
                if s1 == s2 {
                    return s1;
                }

                // A & Blob = A (Blob is top type)
                if matches!(s1, Shape::Blob) {
                    return s2;
                }
                if matches!(s2, Shape::Blob) {
                    return s1;
                }

                // Disjoint concrete types = bottom (Mismatch)
                let is_disjoint = matches!(
                    (&s1, &s2),
                    (Shape::Number(_), Shape::String(_))
                        | (Shape::String(_), Shape::Number(_))
                        | (Shape::Number(_), Shape::Bool(_))
                        | (Shape::Bool(_), Shape::Number(_))
                        | (Shape::String(_), Shape::Bool(_))
                        | (Shape::Bool(_), Shape::String(_))
                        | (
                            Shape::Null,
                            Shape::Number(_) | Shape::String(_) | Shape::Bool(_)
                        )
                        | (
                            Shape::Number(_) | Shape::String(_) | Shape::Bool(_),
                            Shape::Null
                        )
                        | (Shape::Array(_, _), Shape::Object(_))
                        | (Shape::Object(_), Shape::Array(_, _))
                );
                if is_disjoint {
                    return Shape::Mismatch(Box::new(s1), Box::new(s2));
                }

                // Specific & General = Specific
                match (&s1, &s2) {
                    (Shape::Number(Some(n)), Shape::Number(None)) => {
                        return Shape::Number(Some(*n))
                    }
                    (Shape::Number(None), Shape::Number(Some(n))) => {
                        return Shape::Number(Some(*n))
                    }
                    (Shape::String(Some(s)), Shape::String(None)) => {
                        return Shape::String(Some(s.clone()))
                    }
                    (Shape::String(None), Shape::String(Some(s))) => {
                        return Shape::String(Some(s.clone()))
                    }
                    (Shape::Bool(Some(b)), Shape::Bool(None)) => return Shape::Bool(Some(*b)),
                    (Shape::Bool(None), Shape::Bool(Some(b))) => return Shape::Bool(Some(*b)),
                    // Bool(Some(true)) & Bool(Some(false)) -> Mismatch (empty/bottom)
                    (Shape::Bool(Some(true)), Shape::Bool(Some(false)))
                    | (Shape::Bool(Some(false)), Shape::Bool(Some(true))) => {
                        return Shape::Mismatch(Box::new(s1), Box::new(s2))
                    }
                    _ => {}
                }

                Shape::Intersection(Box::new(s1), Box::new(s2))
            }
            Shape::Arrow(input, output) => Shape::Arrow(
                Box::new(input.canonicalize()),
                Box::new(output.canonicalize()),
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
            // A concrete object has exactly these keys, all present: a
            // closed row of required fields.
            Json::Object(obj) => Shape::object_closed(
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
            Filter::ObjIndex(s) => {
                // shapes
                //     .into_iter()
                //     .map(|shape| match shape {
                //         Shape::Blob => {
                //             let new_type_var = ctx.fresh();
                //             ctx.insert(new_type_var, Shape::tvar(new_type_var));
                //             // todo: check this
                //             Shape::tvar(new_type_var)
                //         }
                //         Shape::TVar(t) => {
                //             let new_type_var = ctx.fresh();
                //             let current_shape = ctx.get(&t).unwrap().clone();
                //             // <'I> + {s: <'T>}
                //             let current_shape = Shape::merge_shapes(
                //                 current_shape,
                //                 Shape::Object(vec![(s.clone(), Shape::tvar(new_type_var))]),
                //                 ctx,
                //             );
                //             ctx.insert(t, current_shape);
                //             ctx.insert(new_type_var, Shape::tvar(new_type_var));
                //             Shape::tvar(new_type_var)
                //         }
                //         Shape::Null => Shape::Null,
                //         Shape::Bool(b) => Shape::Mismatch(
                //             Box::new(Shape::Bool(b)),
                //             Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                //         ),
                //         Shape::Number(n) => Shape::Mismatch(
                //             Box::new(Shape::Number(n)),
                //             Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                //         ),
                //         Shape::String(s_) => Shape::Mismatch(
                //             Box::new(Shape::String(s_)),
                //             Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                //         ),
                //         Shape::Array(arr, u) => Shape::Mismatch(
                //             Box::new(Shape::Array(arr, u)),
                //             Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                //         ),
                //         Shape::Tuple(tuple) => Shape::Mismatch(
                //             Box::new(Shape::Tuple(tuple)),
                //             Box::new(Shape::Object(vec![(s.clone(), Shape::Blob)])),
                //         ),
                //         Shape::Object(mut obj) => {
                //             let has_key = obj.iter().any(|(k, _)| k == s);

                //             let new_type_var = ctx.fresh();

                //             if !has_key {
                //                 obj.push((s.clone(), Shape::tvar(new_type_var)));
                //             }

                //             Shape::Object(obj)
                //         }
                //         Shape::Mismatch(_, _) => todo!(),
                //         Shape::Union(_, _) => todo!(),
                //         Shape::Neg(shape) => todo!(),
                //         Shape::Intersection(shape, shape1) => todo!(),
                //     })
                //     .collect()
                todo!()
            }
            Filter::ArrayIndex(u) => {
                // shapes
                //     .into_iter()
                //     .map(|shape| match shape {
                //         Shape::Blob => {
                //             let new_type_var = ctx.fresh();
                //             ctx.insert(new_type_var, Shape::tvar(new_type_var));
                //             Shape::tvar(new_type_var)
                //         }
                //         Shape::TVar(t) => {
                //             let new_type_var = ctx.fresh();
                //             let current_shape = ctx.get(&t).unwrap().clone();
                //             // <'I> + {s: <'T>}
                //             let current_shape = Shape::merge_shapes(
                //                 current_shape,
                //                 Shape::Array(Box::new(Shape::tvar(new_type_var)), Some(*u)),
                //                 ctx,
                //             );

                //             ctx.insert(t, current_shape);
                //             ctx.insert(new_type_var, Shape::tvar(new_type_var));

                //             Shape::tvar(new_type_var)
                //         }
                //         Shape::Null => Shape::Null,
                //         Shape::Bool(b) => Shape::Mismatch(
                //             Box::new(Shape::Bool(b)),
                //             Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                //         ),
                //         Shape::Number(n) => Shape::Mismatch(
                //             Box::new(Shape::Number(n)),
                //             Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                //         ),
                //         Shape::String(s) => Shape::Mismatch(
                //             Box::new(Shape::String(s)),
                //             Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                //         ),
                //         Shape::Array(shape, None) => Shape::Array(shape, Some(*u)),
                //         Shape::Array(shape, Some(u_)) => Shape::Array(shape, Some(*u.max(&u_))),
                //         Shape::Tuple(mut tuple) => {
                //             while (*u).max(0) as usize >= tuple.len() {
                //                 let new_type_var = ctx.fresh();
                //                 tuple.push(Shape::tvar(new_type_var));
                //             }

                //             Shape::Tuple(tuple)
                //         }
                //         Shape::Object(obj) => Shape::Mismatch(
                //             Box::new(Shape::Object(obj)),
                //             Box::new(Shape::Array(Box::new(Shape::Blob), Some(*u))),
                //         ),
                //         Shape::Mismatch(_, _) => todo!(),
                //         Shape::Union(_, _) => todo!(),
                //         Shape::Neg(shape) => todo!(),
                //         Shape::Intersection(shape, shape1) => todo!(),
                //     })
                //     .collect()
                todo!()
            }
            Filter::ArrayIterator => shapes
                .into_iter()
                .flat_map(|shape| match shape {
                    Shape::Blob => {
                        let new_type_var = ctx.fresh();
                        ctx.insert(new_type_var, Shape::tvar(new_type_var));
                        vec![Shape::tvar(new_type_var)]
                    }
                    Shape::TVar(t) => {
                        let new_type_var = ctx.fresh();
                        let current_shape = ctx.get(&t).unwrap().clone();
                        let current_shape = Shape::merge_shapes(
                            current_shape,
                            Shape::Array(Box::new(Shape::tvar(new_type_var)), None),
                            ctx,
                        );
                        ctx.insert(t, current_shape);
                        ctx.insert(new_type_var, Shape::tvar(new_type_var));

                        vec![Shape::tvar(new_type_var)]
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
                    Shape::Array(shape, _) => vec![*shape.clone()],
                    Shape::Tuple(vec) => vec,
                    Shape::Object(vec) => vec.into_iter().map(|f| f.value).collect(),
                    Shape::Mismatch(_, _) => todo!(),
                    Shape::Union(_, _) => todo!(),
                    Shape::Neg(shape) => todo!(),
                    Shape::Arrow(_, _) => vec![Shape::Mismatch(
                        Box::new(shape.clone()),
                        Box::new(Shape::Array(Box::new(Shape::Blob), None)),
                    )],
                    // For intersection types, the result is the intersection of iterating each branch
                    // A & B iterated = (A iterated) & (B iterated)
                    Shape::Intersection(s1, s2) => {
                        // For arrays: get element type intersection
                        match (s1.as_ref(), s2.as_ref()) {
                            (Shape::Array(e1, _), Shape::Array(e2, _)) => {
                                vec![Shape::Intersection(e1.clone(), e2.clone())]
                            }
                            (Shape::Tuple(t1), Shape::Tuple(t2)) => t1
                                .iter()
                                .zip(t2.iter())
                                .map(|(a, b)| {
                                    Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()))
                                })
                                .collect(),
                            // For other cases, fall back to Blob
                            _ => vec![Shape::Blob],
                        }
                    }
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
                    Shape::object_closed(
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
                UnOp::Neg => {
                    let s = Shape::build_shape(filter, shapes, ctx, filters);
                    s.into_iter()
                        .map(|s| match s {
                            Shape::Number(n) => Shape::Number(n.map(|n| -n)),
                            Shape::TVar(t) => {
                                let current_shape = ctx.get(&t).unwrap().clone();
                                let current_shape =
                                    Shape::merge_shapes(current_shape, Shape::Number(None), ctx);
                                ctx.insert(t, current_shape);

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
                                (Shape::TVar(t0), Shape::TVar(t1)) => {
                                    if t0 == t1 {
                                        Shape::tvar(t0)
                                    } else {
                                        let current_shape = ctx.get(&t0).unwrap().clone();
                                        let current_shape = Shape::merge_shapes(
                                            current_shape,
                                            Shape::tvar(t1),
                                            ctx,
                                        );
                                        ctx.insert(t0, current_shape);
                                        Shape::tvar(t0)
                                    }
                                }
                                // null + X
                                // X + null
                                (Shape::Null, s) | (s, Shape::Null) => s,
                                // str + str
                                (Shape::String(s1), Shape::String(s2)) => match (s1, s2) {
                                    (None, None) | (Some(_), None) | (None, Some(_)) => {
                                        Shape::String(None)
                                    }
                                    (Some(s1), Some(s2)) => {
                                        Shape::String(Some(format!("{s1}{s2}")))
                                    }
                                },
                                (Shape::String(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::String(_)) => {
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Union(
                                            Box::new(Shape::String(None)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Union(
                                            Box::new(Shape::Array(shape.clone(), u)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

                                    Shape::Array(shape, u)
                                }
                                // obj + obj
                                (Shape::Object(obj1), Shape::Object(obj2)) => {
                                    // todo: check this
                                    let mut obj = HashMap::new();

                                    for f1 in obj1.clone() {
                                        let s2 = obj2.get(&f1.key).map(|f| f.value.clone());

                                        if let Some(s2) = s2 {
                                            obj.insert(
                                                f1.key,
                                                Shape::merge_shapes(f1.value, s2, ctx),
                                            );
                                        } else {
                                            obj.insert(f1.key, f1.value);
                                        }
                                    }

                                    for f2 in obj2 {
                                        let s1 = obj1.get(&f2.key).map(|f| f.value.clone());

                                        if let Some(s1) = s1 {
                                            obj.insert(
                                                f2.key,
                                                Shape::merge_shapes(s1, f2.value, ctx),
                                            );
                                        } else {
                                            obj.insert(f2.key, f2.value);
                                        }
                                    }

                                    Shape::object(obj.into_iter().collect())
                                }
                                (Shape::Object(obj), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Object(obj)) => {
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Object(obj.clone())),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

                                    Shape::Object(obj)
                                }
                                // num + num
                                (Shape::Number(n1), Shape::Number(n2)) => match (n1, n2) {
                                    (None, None) | (Some(_), None) | (None, Some(_)) => {
                                        Shape::Number(None)
                                    }
                                    (Some(n1), Some(n2)) => Shape::Number(Some(n1 + n2)),
                                },
                                (Shape::Number(_), Shape::TVar(t))
                                | (Shape::TVar(t), Shape::Number(_)) => {
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Number(None)),
                                            Box::new(Shape::Null),
                                        ),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape,
                                        Shape::Array(shape.clone(), u),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Number(None),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Union(
                                            Box::new(Shape::Number(None)),
                                            Box::new(Shape::String(None)),
                                        ),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                                    let current_shape = ctx.get(&t).unwrap().clone();
                                    let current_shape = Shape::merge_shapes(
                                        current_shape.clone(),
                                        Shape::Number(None),
                                        ctx,
                                    );
                                    ctx.insert(t, current_shape);

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
                BinOp::Eq => {
                    let s1 = Shape::build_shape(l, shapes.clone(), ctx, filters);
                    let s2 = Shape::build_shape(r, shapes, ctx, filters);

                    s1.into_iter()
                        .zip(s2)
                        .map(|(l, r)| {
                            match (l, r) {
                                (Shape::Mismatch(l, r), _) | (_, Shape::Mismatch(l, r)) => {
                                    Shape::Mismatch(l, r)
                                }
                                // Const computations
                                (Shape::Number(Some(n1)), Shape::Number(Some(n2))) if n1 == n2 => {
                                    Shape::Bool(Some(true))
                                }
                                (Shape::Bool(Some(b1)), Shape::Bool(Some(b2))) if b1 == b2 => {
                                    Shape::Bool(Some(true))
                                }
                                (Shape::String(Some(s1)), Shape::String(Some(s2))) if s1 == s2 => {
                                    Shape::Bool(Some(true))
                                }
                                (Shape::Null, Shape::Null) => Shape::Bool(Some(true)),
                                // Imprecise computations
                                (Shape::Number(Some(_)), Shape::Number(None))
                                | (Shape::Number(None), Shape::Number(Some(_)))
                                | (Shape::Number(None), Shape::Number(None))
                                | (Shape::Bool(Some(_)), Shape::Bool(None))
                                | (Shape::Bool(None), Shape::Bool(Some(_)))
                                | (Shape::String(Some(_)), Shape::String(None))
                                | (Shape::String(None), Shape::String(Some(_))) => {
                                    Shape::Bool(None)
                                }
                                // Refine TVar's to respect the equality check
                                (Shape::TVar(t), s) | (s, Shape::TVar(t)) => {
                                    // return a condition
                                    // Shape::union(Shape::cond(), s2)
                                    todo!()
                                }
                                _ => todo!(),
                            }
                        })
                        .collect()
                }
            },
            Filter::Empty => vec![],
            Filter::Error => {
                // if the input shape is a type variable, invert it; if it is a concrete type, return a mismatch
                shapes
                    .into_iter()
                    .map(|shape| match shape {
                        Shape::TVar(t) => {
                            let current_shape = ctx.get(&t).unwrap().clone();
                            ctx.insert(t, Shape::Neg(Box::new(current_shape)));
                            Shape::Neg(Box::new(Shape::tvar(t)))
                        }
                        _ => Shape::Mismatch(Box::new(shape.clone()), Box::new(Shape::neg(shape))),
                    })
                    .collect()
            }
            Filter::Call(name, filters_) => match filters_ {
                Some(args) => {
                    let filter = filters
                        .get(name)
                        .unwrap_or_else(|| panic!("filter {name} not found"));
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
                tracing::debug!("computing if_then_else {s:?} {s1:?} {s2:?}");
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
            Filter::Bound(_, filter) => {
                // todo: understand this better
                Shape::build_shape(filter, shapes, ctx, filters)
            }
            Filter::FunctionExpression(_, expr) => {
                let shapes = Shape::build_shape(expr, shapes, ctx, filters);
                shapes
                    .into_iter()
                    .map(|s| match s {
                        Shape::Blob => {
                            let new_type_var = ctx.fresh();
                            ctx.insert(new_type_var, Shape::TVar(new_type_var));
                            Shape::TVar(new_type_var)
                        }
                        Shape::TVar(t) => {
                            let current_shape = ctx.get(&t).unwrap().clone();
                            let current_shape =
                                Shape::merge_shapes(current_shape, Shape::Blob, ctx);
                            ctx.insert(t, current_shape);
                            Shape::TVar(t)
                        }
                        _ => s,
                    })
                    .collect()
            }
            Filter::BindingExpression(_, _) => todo!(),
            Filter::Variable(_) => todo!(),
            Filter::ReduceExpression(_, _, _, _) => todo!(),
            Filter::Hole => todo!(),
            Filter::SliceExpression(_, _) => {
                // Slicing preserves the type: array -> array, string -> string
                shapes
            }
        }
    }

    pub fn merge_shapes(s1: Shape, s2: Shape, ctx: &mut ShapeContext) -> Shape {
        match (s1, s2) {
            (Shape::Blob, s) | (s, Shape::Blob) => s,
            (Shape::TVar(t0), Shape::TVar(t1)) => {
                if t0 == t1 {
                    Shape::tvar(t0)
                } else {
                    let current_shape = ctx.get(&t0).unwrap().clone();
                    let current_shape = Shape::merge_shapes(current_shape, Shape::tvar(t1), ctx);
                    ctx.insert(t0, current_shape);
                    Shape::tvar(t0)
                }
            }
            (Shape::TVar(t), s) | (s, Shape::TVar(t)) => {
                let current_shape = ctx.get(&t).unwrap().clone();
                match current_shape {
                    Shape::Blob => {
                        ctx.insert(t, s.clone());
                        s
                    }
                    Shape::TVar(t1) => {
                        ctx.insert(t1, s.clone());
                        s
                    }
                    _ => {
                        let current_shape = Shape::merge_shapes(current_shape, s.clone(), ctx);
                        ctx.insert(t, current_shape.clone());
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
                    tuple1.push(Shape::tvar(new_type_var));
                }

                while tuple2.len() < len {
                    let new_type_var = ctx.fresh();
                    tuple2.push(Shape::tvar(new_type_var));
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

                for f1 in obj1.clone() {
                    let s2 = obj2.get(&f1.key).map(|f| f.value.clone());

                    if let Some(s2) = s2 {
                        obj.insert(f1.key, Shape::merge_shapes(f1.value, s2, ctx));
                    } else {
                        obj.insert(f1.key, f1.value);
                    }
                }

                for f2 in obj2 {
                    let s1 = obj1.get(&f2.key).map(|f| f.value.clone());

                    if let Some(s1) = s1 {
                        obj.insert(f2.key, Shape::merge_shapes(s1, f2.value, ctx));
                    } else {
                        obj.insert(f2.key, f2.value);
                    }
                }

                Shape::object(obj.into_iter().collect())
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
                if let Json::Object(obj) = &j {
                    let obj_map = obj.clone().into_iter().collect::<HashMap<String, Json>>();

                    // Each field: a required key must be present and conform;
                    // an optional key, if present, must conform; absence is
                    // fine for optional fields.
                    for field in obj_shape.iter() {
                        match obj_map.get(&field.key) {
                            Some(v) => {
                                if let Some(m) = Shape::check(
                                    &field.value,
                                    v.clone(),
                                    [path.clone(), vec![Access::Field(field.key.clone())]].concat(),
                                ) {
                                    return Some(m);
                                }
                            }
                            None if field.optional => {}
                            None => {
                                return Some(ShapeMismatch::new(
                                    path,
                                    self.clone(),
                                    Shape::from_json(j),
                                ));
                            }
                        }
                    }

                    // A closed row forbids any key beyond its fields.
                    if !obj_shape.open {
                        let has_extra = obj.iter().any(|(k, _)| obj_shape.get(k).is_none());
                        if has_extra {
                            return Some(ShapeMismatch::new(
                                path,
                                self.clone(),
                                Shape::from_json(j),
                            ));
                        }
                    }

                    None
                } else {
                    Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j)))
                }
            }
            Shape::Mismatch(s1, s2) => Some(ShapeMismatch::new(path, *s1.clone(), *s2.clone())),
            Shape::Union(s1, s2) => {
                let (matches, mismatches): (Vec<_>, Vec<_>) = [s1, s2]
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
            Shape::Neg(shape) => {
                let inner_result = shape.check(j.clone(), path.clone());
                match inner_result {
                    None => Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j))),
                    Some(_) => None,
                }
            }
            // Intersection: value must satisfy BOTH types
            Shape::Intersection(s1, s2) => {
                let check1 = s1.check(j.clone(), path.clone());
                let check2 = s2.check(j, path);
                // Both must pass (return None) for intersection to pass
                check1.or(check2)
            }
            // Arrow types are not JSON values; checking a JSON value against a function
            // type is a category mismatch.
            Shape::Arrow(_, _) => Some(ShapeMismatch::new(path, self.clone(), Shape::from_json(j))),
        }
    }

    pub fn check_self(&self, path: Vec<Access>) -> Option<ShapeMismatch> {
        match self {
            Shape::TVar(_) => None,
            Shape::Blob | Shape::Null | Shape::Bool(..) | Shape::Number(..) | Shape::String(..) => {
                None
            }
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
                    .map(|field| {
                        field.value.check_self(
                            [path.clone(), vec![Access::Field(field.key.clone())]].concat(),
                        )
                    })
                    .partition(Option::is_none);

                mismatches.into_iter().next().flatten()
            }
            Shape::Mismatch(s1, s2) => Some(ShapeMismatch::new(path, *s1.clone(), *s2.clone())),
            Shape::Union(s1, s2) => {
                let (_, mismatches): (Vec<_>, Vec<_>) = [s1, s2]
                    .iter()
                    .map(|s| s.check_self(path.clone()))
                    .partition(Option::is_none);

                mismatches.into_iter().next().flatten()
            }
            Shape::Neg(shape) => {
                let inner_result = shape.check_self(path.clone());
                match inner_result {
                    None => Some(ShapeMismatch::new(path, self.clone(), self.clone())),
                    Some(_) => None,
                }
            }
            // Intersection: check both components for self-consistency
            Shape::Intersection(s1, s2) => {
                let check1 = s1.check_self(path.clone());
                let check2 = s2.check_self(path);
                check1.or(check2)
            }
            // Arrow self-check: each side must be internally consistent.
            Shape::Arrow(input, output) => input
                .check_self(path.clone())
                .or_else(|| output.check_self(path)),
        }
    }
}

#[cfg(test)]
mod shape_computation_tests {
    use crate::shape::{Access, DirectInference, Shape, ShapeMismatch};
    use crate::TypeInference;
    use std::collections::HashMap;
    use tjq_exec::{filters, parse, Filter, Json};

    fn builtin_filters() -> HashMap<String, Filter> {
        // read defs.jq
        filters(include_str!("../tjq/defs.jq"))
    }

    /// Check if a shape is any type variable (regardless of number)
    fn is_tvar(shape: &Shape) -> bool {
        matches!(shape, Shape::TVar(_))
    }

    /// Check if two shapes are the same type variable
    fn same_tvar(s1: &Shape, s2: &Shape) -> bool {
        match (s1, s2) {
            (Shape::TVar(a), Shape::TVar(b)) => a == b,
            _ => false,
        }
    }

    /// Check structural equivalence of shapes, ignoring specific TVar numbers.
    /// Two shapes are equivalent if they have the same structure and TVars
    /// that should be equal are equal (by same number), but we don't care
    /// what the actual numbers are.
    fn shapes_structurally_equal(s1: &Shape, s2: &Shape) -> bool {
        match (s1, s2) {
            (Shape::TVar(_), Shape::TVar(_)) => true, // Any TVar matches any TVar structurally
            (Shape::Blob, Shape::Blob) => true,
            (Shape::Null, Shape::Null) => true,
            (Shape::Bool(a), Shape::Bool(b)) => a == b,
            (Shape::Number(a), Shape::Number(b)) => a == b,
            (Shape::String(a), Shape::String(b)) => a == b,
            (Shape::Array(a, n1), Shape::Array(b, n2)) => {
                n1 == n2 && shapes_structurally_equal(a, b)
            }
            (Shape::Tuple(a), Shape::Tuple(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| shapes_structurally_equal(x, y))
            }
            (Shape::Object(a), Shape::Object(b)) => {
                a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(f1, f2)| {
                        f1.key == f2.key && shapes_structurally_equal(&f1.value, &f2.value)
                    })
            }
            (Shape::Union(a1, a2), Shape::Union(b1, b2)) => {
                shapes_structurally_equal(a1, b1) && shapes_structurally_equal(a2, b2)
            }
            (Shape::Neg(a), Shape::Neg(b)) => shapes_structurally_equal(a, b),
            (Shape::Mismatch(a1, a2), Shape::Mismatch(b1, b2)) => {
                shapes_structurally_equal(a1, b1) && shapes_structurally_equal(a2, b2)
            }
            _ => false,
        }
    }

    /// Extract all TVars from a shape
    fn collect_tvars(shape: &Shape) -> Vec<usize> {
        match shape {
            Shape::TVar(n) => vec![*n],
            Shape::Array(s, _) => collect_tvars(s),
            Shape::Tuple(shapes) => shapes.iter().flat_map(collect_tvars).collect(),
            Shape::Object(fields) => fields
                .iter()
                .flat_map(|f| collect_tvars(&f.value))
                .collect(),
            Shape::Union(a, b) | Shape::Mismatch(a, b) => {
                let mut v = collect_tvars(a);
                v.extend(collect_tvars(b));
                v
            }
            Shape::Neg(s) => collect_tvars(s),
            _ => vec![],
        }
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

    #[test]
    fn test_add_dot_number() {
        let (shape, result) = get_type("1 + .");
        assert_eq!(shape, Shape::union(Shape::number_(), Shape::null()));
        assert_eq!(result, Shape::number_());
    }

    #[test]
    fn test_add_dot_dot() {
        let (shape, result) = get_type(". + .");
        // Input and output should be the same type variable (identity on input type)
        assert!(
            same_tvar(&shape, &result),
            "input and output should be same TVar"
        );
    }

    #[test]
    fn test_plus1() {
        let json = json("1.0");
        let (shape, result) = get_type("1 + .");

        assert_eq!(shape, Shape::union(Shape::number_(), Shape::null()));
        assert_eq!(result, Shape::number_());

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    #[test]
    fn test_plus2() {
        let (shape, results) = get_type_with_results(". + 1, . + \"hello\"");

        assert_eq!(
            shape,
            Shape::mismatch(
                Shape::union(Shape::number_(), Shape::null()),
                Shape::union(Shape::string_(), Shape::null())
            )
        );

        assert_eq!(results.len(), 2);

        assert_eq!(results.first().unwrap(), &Shape::number_());

        assert_eq!(results.last().unwrap(), &Shape::string_());
    }

    #[test]
    fn test_minus1() {
        let json = json("1.0");
        let (shape, result) = get_type("1 - 2");

        // Input is unused, so it's a type variable (any type)
        assert!(is_tvar(&shape), "input should be a TVar (unused)");
        assert_eq!(result, Shape::number(-1.0));

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    #[test]
    fn test_minus2() {
        let json1 = json("1.0");
        let json2 = json(r#"{"a": 2.0}"#);
        let (shape, result) = get_type("1 - .a");

        assert_eq!(
            shape,
            Shape::object(vec![("a".to_string(), Shape::number_())])
        );

        assert_eq!(result, Shape::number_());

        let results = shape.check(json1, vec![]);

        assert_eq!(
            results,
            Some(ShapeMismatch::new(
                vec![],
                Shape::object(vec![("a".to_string(), Shape::number_())]),
                Shape::number(1.0),
            ))
        );

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_minus3() {
        // "x" - .a
        let (shape, result) = get_type("\"x\" - .a");

        assert_eq!(shape, Shape::object(vec![("a".to_string(), Shape::blob())]));
        assert_eq!(result, Shape::mismatch(Shape::string("x"), Shape::blob()));
    }

    #[test]
    fn test_abs() {
        let (shape, result) = get_type("abs");
        assert_eq!(shape, Shape::number_());
        assert_eq!(result.canonicalize(), Shape::number_());
    }

    #[test]
    fn test_isboolean() {
        let json1 = json("true");
        let json2 = json("1.0");
        let (shape, result) = get_type("isboolean");

        assert!(is_tvar(&shape), "input should be a type variable");
        assert_eq!(result, Shape::bool_());

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_type() {
        let json1 = json("true");
        let json2 = json("1.0");
        let (shape, result) = get_type("type");

        assert!(is_tvar(&shape), "input should be a type variable");

        assert_eq!(
            result,
            Shape::union(
                Shape::string("null"),
                Shape::union(
                    Shape::string("boolean"),
                    Shape::union(
                        Shape::string("number"),
                        Shape::union(
                            Shape::string("string"),
                            Shape::union(Shape::string("array"), Shape::string("object"))
                        )
                    )
                )
            )
        );

        let results = shape.check(json1, vec![]);
        assert_eq!(results, None);

        let results = shape.check(json2, vec![]);
        assert_eq!(results, None);
    }

    #[test]
    fn test_map1() {
        let json1 = json("[-1.0, 2.0]");
        let json2 = json(r#"["hello", "world"]"#);
        let json3 = json(r#"[-1.0, "world"]"#);
        let json4 = json(r#"["hello", true]"#);

        // map(. * 2)
        let (shape, result) = get_type("map(. * 2)");
        assert_eq!(
            shape,
            Shape::array(Shape::union(Shape::number_(), Shape::string_()), None)
        );

        assert_eq!(
            result,
            Shape::array(Shape::union(Shape::number_(), Shape::string_()), None)
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
                Shape::union(Shape::number_(), Shape::string_()),
                Shape::bool(true)
            ))
        );
    }

    #[test]
    fn test_map2() {
        let json1 = json("[-1.0, 2.0]");
        let json2 = json(r#"["hello", "world"]"#);
        let json3 = json(r#"[-1.0, "world"]"#);
        let json4 = json("[null, true]");

        let (shape, result) = get_type("map(. + 2)");

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

        assert_eq!(result, Shape::Array(Box::new(Shape::Number(None)), None));

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
        let json = json("[[-1.0, -2.0], [3.0, 4.0]]");
        let (shape, result) = get_type("map(map(abs))");

        assert_eq!(
            shape,
            Shape::Array(
                Box::new(Shape::Array(Box::new(Shape::Number(None)), None)),
                None
            )
        );

        assert_eq!(
            result.canonicalize(),
            Shape::Array(
                Box::new(Shape::Array(Box::new(Shape::Number(None)), None)),
                None
            )
        );

        let results = shape.check(json, vec![]);

        assert_eq!(results, None);
    }

    fn get_type_with_results(expression: &str) -> (Shape, Vec<Shape>) {
        get_type_with_results_using(&DirectInference, expression)
    }

    fn get_type(expression: &str) -> (Shape, Shape) {
        get_type_using(&DirectInference, expression)
    }

    fn get_type_with_results_using(
        inference: &impl TypeInference,
        expression: &str,
    ) -> (Shape, Vec<Shape>) {
        // Initialize tracing subscriber if not already initialized
        let _ = tracing_subscriber::fmt()
            .with_target(false)
            .with_thread_ids(false)
            .with_thread_names(false)
            .with_file(true)
            .with_line_number(true)
            .with_level(true)
            .without_time()
            .with_max_level(tracing::Level::DEBUG)
            .try_init();

        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let (i, o) = inference.infer(&filter, &builtin_filters());
        (
            i.canonicalize(),
            o.into_iter().map(|s| s.canonicalize()).collect(),
        )
    }

    fn get_type_using(inference: &impl TypeInference, expression: &str) -> (Shape, Shape) {
        // Initialize tracing subscriber if not already initialized
        let _ = tracing_subscriber::fmt()
            .with_target(false)
            .with_thread_ids(false)
            .with_thread_names(false)
            .with_file(true)
            .with_line_number(true)
            .with_level(true)
            .without_time()
            .with_max_level(tracing::Level::DEBUG)
            .try_init();

        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let (i, o) = inference.infer(&filter, &builtin_filters());
        (i.canonicalize(), o[0].canonicalize())
    }

    #[test]
    fn test_solver_dot() {
        let (tin, tout) = get_type(r#"."#);
        // t: T -> T (both should be the same type variable)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert!(
            same_tvar(&tin, &tout),
            "input and output should be the same type variable"
        );
    }

    #[test]
    fn test_solver_number() {
        let (tin, tout) = get_type(r#"3"#);
        // t: T -> Number (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::number(3.0));
    }

    #[test]
    fn test_solver_boolean() {
        let (tin, tout) = get_type(r#"true"#);
        // t: T -> Bool (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::bool(true));
    }

    #[test]
    fn test_solver_string() {
        let (tin, tout) = get_type(r#""hello""#);
        // t: T -> String (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::string("hello"));
    }

    #[test]
    fn test_solver_array() {
        let (tin, tout) = get_type(r#"[1, 2, 3]"#);
        // t: T -> [Number, Number, Number] (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(
            tout,
            Shape::tuple(vec![
                Shape::number(1.0),
                Shape::number(2.0),
                Shape::number(3.0)
            ])
        );
    }

    #[test]
    fn test_solver_object() {
        let (tin, tout) = get_type(r#"{ "a": 1, "b": 2 }"#);
        // t: T -> {a: Number, b: Number} (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(
            tout,
            Shape::object(vec![
                ("a".to_string(), Shape::number(1.0)),
                ("b".to_string(), Shape::number(2.0))
            ])
        );
    }

    #[test]
    #[ignore]
    fn test_solver_not() {
        let (tin, tout) = get_type(r#"not"#);
        // t: T -> T
        assert_eq!(tin, Shape::union(Shape::null(), Shape::bool_()));
        assert_eq!(tout, Shape::union(Shape::null(), Shape::bool_()));
    }

    #[test]
    fn test_solver_negation() {
        let (tin, tout) = get_type(r#"- ."#);
        // t: T -> T
        assert_eq!(tin, Shape::number_());
        assert_eq!(tout, Shape::number_());
    }

    #[test]
    #[ignore = "We cannot actually do type-level computation until we can create constraints that do computation somehow"]
    fn test_solver_add_definite() {
        let (tin, tout) = get_type(r#"1 + 1"#);
        // t: T -> Number (input is unconstrained)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::number(2.0));
    }

    #[test]
    #[ignore = "We can't solve subtyping constraints yet"]
    fn test_solver_math() {
        let (tin, tout) = get_type(r#". + 1"#);
        // t: T -> T
        assert_eq!(tin, Shape::union(Shape::null(), Shape::number_()));
        assert_eq!(tout, Shape::number_());
    }

    #[test]
    #[ignore]
    fn test_solver_pipe() {
        let (tin, tout) = get_type(r#".a | .b"#);
        // t: { a: { b: T }} -> T
        // Check tin has structure {a: {b: TVar}}
        if let Shape::Object(fields) = &tin {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].key, "a");
            if let Shape::Object(inner_fields) = &fields[0].value {
                assert_eq!(inner_fields.len(), 1);
                assert_eq!(inner_fields[0].key, "b");
                let inner_tvar = &inner_fields[0].value;
                assert!(is_tvar(inner_tvar), "inner field should be a type variable");
                // tout should be the same TVar as the nested one
                assert!(
                    same_tvar(inner_tvar, &tout),
                    "output should be the same type variable as the nested field"
                );
            } else {
                panic!("inner field 'a' should be an object");
            }
        } else {
            panic!("input should be an object");
        }
    }

    #[test]
    fn test_error_neg() {
        let (tin, tout) = get_type(r#"if . == true or . == false then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::bool_());
        assert_eq!(tout, Shape::number(1.0));
    }
}

/// Tests for the experimental constraint-based type inference
#[cfg(test)]
mod constraint_inference_tests {
    use crate::experimental_type_inference::ConstraintInference;
    use crate::shape::Shape;
    use crate::TypeInference;
    use std::collections::HashMap;
    use tjq_exec::{filters, parse, Filter};

    fn builtin_filters() -> HashMap<String, Filter> {
        filters(include_str!("../tjq/defs.jq"))
    }

    fn is_tvar(shape: &Shape) -> bool {
        matches!(shape, Shape::TVar(_))
    }

    fn same_tvar(s1: &Shape, s2: &Shape) -> bool {
        match (s1, s2) {
            (Shape::TVar(a), Shape::TVar(b)) => a == b,
            _ => false,
        }
    }

    fn get_type(expression: &str) -> (Shape, Shape) {
        get_type_using(&ConstraintInference, expression)
    }

    fn get_type_using(inference: &impl TypeInference, expression: &str) -> (Shape, Shape) {
        let _ = tracing_subscriber::fmt()
            .with_target(false)
            .with_thread_ids(false)
            .with_thread_names(false)
            .with_file(true)
            .with_line_number(true)
            .with_level(true)
            .without_time()
            .with_max_level(tracing::Level::DEBUG)
            .try_init();

        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let (i, o) = inference.infer(&filter, &builtin_filters());
        (i.canonicalize(), o[0].canonicalize())
    }

    #[test]
    fn test_constraint_solver_dot() {
        let (tin, tout) = get_type(r#"."#);
        // t: T -> T (both should be the same type variable)
        assert!(is_tvar(&tin), "input should be a type variable");
        assert!(
            same_tvar(&tin, &tout),
            "input and output should be the same type variable"
        );
    }

    #[test]
    fn test_constraint_solver_number() {
        let (tin, tout) = get_type(r#"3"#);
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::number(3.0));
    }

    #[test]
    fn test_constraint_solver_boolean() {
        let (tin, tout) = get_type(r#"true"#);
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::bool(true));
    }

    #[test]
    fn test_constraint_solver_string() {
        let (tin, tout) = get_type(r#""hello""#);
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::string("hello"));
    }

    #[test]
    fn test_constraint_solver_array() {
        let (tin, tout) = get_type(r#"[1, 2, 3]"#);
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(
            tout,
            Shape::tuple(vec![
                Shape::number(1.0),
                Shape::number(2.0),
                Shape::number(3.0)
            ])
        );
    }

    #[test]
    fn test_constraint_solver_object() {
        let (tin, tout) = get_type(r#"{ "a": 1, "b": 2 }"#);
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(
            tout,
            // object construction produces a closed row (exactly these keys)
            Shape::object_closed(vec![
                ("a".to_string(), Shape::number(1.0)),
                ("b".to_string(), Shape::number(2.0))
            ])
        );
    }

    #[test]
    fn test_constraint_solver_negation() {
        let (tin, tout) = get_type(r#"- ."#);
        assert_eq!(tin, Shape::number_());
        assert_eq!(tout, Shape::number_());
    }

    #[test]
    fn test_constraint_solver_math_dot_dot() {
        let (tin, tout) = get_type(r#". + ."#);
        // `+` accepts numbers, strings, arrays (concat), objects (merge),
        // and null (identity); union nesting order is not semantically
        // meaningful, so compare via mutual inclusion.
        let addable = Shape::union(
            Shape::Null,
            Shape::union(
                Shape::union(Shape::number_(), Shape::string_()),
                Shape::union(
                    Shape::Array(Box::new(Shape::Blob), None),
                    Shape::object(vec![]),
                ),
            ),
        );
        assert!(
            tin.included_in(&addable) && addable.included_in(&tin),
            "expected {addable}, got {tin}"
        );
        // Output covers the same set (null + null yields null in jq)
        assert!(
            tout.included_in(&addable) && addable.included_in(&tout),
            "expected {addable}, got {tout}"
        );
    }

    #[test]
    fn test_constraint_solver_math_with_number() {
        let (tin, tout) = get_type(r#". + 1"#);
        // Constraint solver infers union with null
        assert_eq!(tin, Shape::union(Shape::number_(), Shape::Null));
        // Output joins the number branch (number) with the null branch (1);
        // number | 1 canonicalizes to number.
        assert_eq!(tout, Shape::number_());
    }

    #[test]
    fn test_constraint_solver_pipe() {
        let (tin, tout) = get_type(r#".a | .b"#);
        // Constraint solver infers nested object structure: {a: {b: T}} -> T
        // Check tin has structure {a: {b: TVar}}
        if let Shape::Object(fields) = &tin {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].key, "a");
            if let Shape::Object(inner_fields) = &fields[0].value {
                assert_eq!(inner_fields.len(), 1);
                assert_eq!(inner_fields[0].key, "b");
                let inner_tvar = &inner_fields[0].value;
                assert!(is_tvar(inner_tvar), "inner field should be a type variable");
                // tout should be the same TVar as the nested one
                assert!(
                    same_tvar(inner_tvar, &tout),
                    "output should be the same type variable as the nested field"
                );
            } else {
                panic!("inner field 'a' should be an object");
            }
        } else {
            panic!("input should be an object");
        }
    }

    #[test]
    fn test_constraint_solver_add_definite() {
        let (tin, tout) = get_type(r#"1 + 1"#);
        // Constraint solver can compute constant expressions
        assert!(is_tvar(&tin), "input should be a type variable");
        assert_eq!(tout, Shape::number(2.0));
    }

    #[test]
    fn test_constraint_error_neg() {
        let (tin, tout) = get_type(r#"if . == true or . == false then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::bool_());
        assert_eq!(tout, Shape::number(1.0));
    }

    #[test]
    fn test_constraint_error_neq() {
        let (tin, tout) = get_type(r#"if . != 5 then 1 else error end"#);
        assert_eq!(tin, Shape::number(5).neg());
        assert_eq!(tout, Shape::number(1.0));
    }

    #[test]
    fn test_constraint_obj_index() {
        let (tin, tout) = get_type(r#".foo"#);
        // Constraint solver infers object with field: {foo: T} -> T
        if let Shape::Object(fields) = &tin {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].key, "foo");
            let field_tvar = &fields[0].value;
            assert!(is_tvar(field_tvar), "field should be a type variable");
            assert!(
                same_tvar(field_tvar, &tout),
                "output should be the same type variable as the field"
            );
        } else {
            panic!("input should be an object");
        }
    }
}
