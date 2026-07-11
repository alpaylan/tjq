//! A reduced, ordered Binary Decision Diagram (ROBDD) over type *atoms*, the
//! representation set-theoretic type systems (Frisch–Castagna semantic
//! subtyping, as in CDuce and the Elixir type checker) use for the boolean
//! structure of types. A type is a boolean combination of atoms; the BDD makes
//! union/intersection/negation canonical and cheap, and is the substrate for a
//! decidable emptiness/subtyping test.
//!
//! This module provides the boolean core over the *base*-type fragment
//! (`null`, booleans, numbers, strings, and the coarse array/object kinds),
//! validated denotationally against `Shape::check`. The container-recursive
//! atoms (products/records carrying sub-types) that full semantic subtyping
//! needs are future work; see `docs/type-system-scope.md`.

use std::cmp::Ordering;
use std::rc::Rc;

use tjq_exec::Json;

use crate::shape::Shape;

/// A primitive positive predicate on a JSON value. Atoms are totally ordered so
/// the BDD can keep a canonical variable order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    Null,
    /// Any boolean, or a specific one.
    Bool(Option<bool>),
    /// Any number, or a specific one (compared by bit pattern for a total order).
    Number(Option<u64>),
    /// Any string, or a specific one.
    String(Option<String>),
    /// The array kind (element type not modelled here — coarse).
    Array,
    /// The object kind (fields not modelled here — coarse).
    Object,
}

impl Atom {
    fn number(n: f64) -> Atom {
        Atom::Number(Some(n.to_bits()))
    }

    /// Does `v` satisfy this atomic predicate?
    pub fn holds(&self, v: &Json) -> bool {
        match self {
            Atom::Null => matches!(v, Json::Null),
            Atom::Bool(None) => matches!(v, Json::Boolean(_)),
            Atom::Bool(Some(b)) => matches!(v, Json::Boolean(x) if x == b),
            Atom::Number(None) => matches!(v, Json::Number(_)),
            Atom::Number(Some(bits)) => matches!(v, Json::Number(x) if x.to_bits() == *bits),
            Atom::String(None) => matches!(v, Json::String(_)),
            Atom::String(Some(s)) => matches!(v, Json::String(x) if x == s),
            Atom::Array => matches!(v, Json::Array(_)),
            Atom::Object => matches!(v, Json::Object(_)),
        }
    }

    /// A total order used for BDD variable ordering (kind, then value).
    fn rank(&self) -> (u8, Option<Vec<u8>>) {
        match self {
            Atom::Null => (0, None),
            Atom::Bool(b) => (1, Some(vec![b.map_or(2, |x| x as u8)])),
            Atom::Number(n) => (2, Some(n.map_or(vec![], |b| b.to_be_bytes().to_vec()))),
            Atom::String(s) => (
                3,
                Some(s.as_ref().map_or(vec![], |x| x.as_bytes().to_vec())),
            ),
            Atom::Array => (4, None),
            Atom::Object => (5, None),
        }
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Atom {
    fn cmp(&self, other: &Self) -> Ordering {
        self.rank().cmp(&other.rank())
    }
}

/// A BDD node. `Leaf(true)` is the top type (⊤), `Leaf(false)` the empty type
/// (⊥). An `If` node branches on `atom`: `hi` when the atom holds, `lo` when it
/// does not. Reduced (`hi != lo`) and ordered (`atom` precedes every atom in
/// its children), so structural equality is semantic equality of the boolean
/// combination.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bdd {
    Leaf(bool),
    If {
        atom: Atom,
        hi: Rc<Bdd>,
        lo: Rc<Bdd>,
    },
}

impl Bdd {
    pub fn top() -> Rc<Bdd> {
        Rc::new(Bdd::Leaf(true))
    }
    pub fn bottom() -> Rc<Bdd> {
        Rc::new(Bdd::Leaf(false))
    }

    /// The type denoted by a single atom.
    pub fn atom(a: Atom) -> Rc<Bdd> {
        mk(a, Bdd::bottom(), Bdd::top())
    }

    /// Membership: does `v` inhabit the type this BDD denotes?
    pub fn holds(&self, v: &Json) -> bool {
        match self {
            Bdd::Leaf(b) => *b,
            Bdd::If { atom, hi, lo } => {
                if atom.holds(v) {
                    hi.holds(v)
                } else {
                    lo.holds(v)
                }
            }
        }
    }
}

/// Smart constructor: reduce (collapse `hi == lo`) and share.
fn mk(atom: Atom, lo: Rc<Bdd>, hi: Rc<Bdd>) -> Rc<Bdd> {
    if lo == hi {
        lo
    } else {
        Rc::new(Bdd::If { atom, hi, lo })
    }
}

/// Logical negation (swap the leaves).
pub fn not(a: &Rc<Bdd>) -> Rc<Bdd> {
    match a.as_ref() {
        Bdd::Leaf(b) => Rc::new(Bdd::Leaf(!b)),
        Bdd::If { atom, hi, lo } => mk(atom.clone(), not(lo), not(hi)),
    }
}

/// Binary apply over the canonical atom order; `f` combines the leaves.
fn apply(a: &Rc<Bdd>, b: &Rc<Bdd>, f: fn(bool, bool) -> bool) -> Rc<Bdd> {
    match (a.as_ref(), b.as_ref()) {
        (Bdd::Leaf(x), Bdd::Leaf(y)) => Rc::new(Bdd::Leaf(f(*x, *y))),
        (Bdd::Leaf(_), Bdd::If { atom, hi, lo }) => {
            mk(atom.clone(), apply(a, lo, f), apply(a, hi, f))
        }
        (Bdd::If { atom, hi, lo }, Bdd::Leaf(_)) => {
            mk(atom.clone(), apply(lo, b, f), apply(hi, b, f))
        }
        (
            Bdd::If { atom: aa, hi: ah, lo: al },
            Bdd::If { atom: ba, hi: bh, lo: bl },
        ) => match aa.cmp(ba) {
            Ordering::Equal => mk(aa.clone(), apply(al, bl, f), apply(ah, bh, f)),
            Ordering::Less => mk(aa.clone(), apply(al, b, f), apply(ah, b, f)),
            Ordering::Greater => mk(ba.clone(), apply(a, bl, f), apply(a, bh, f)),
        },
    }
}

pub fn or(a: &Rc<Bdd>, b: &Rc<Bdd>) -> Rc<Bdd> {
    apply(a, b, |x, y| x || y)
}
pub fn and(a: &Rc<Bdd>, b: &Rc<Bdd>) -> Rc<Bdd> {
    apply(a, b, |x, y| x && y)
}

/// Collect every atom mentioned in a BDD.
fn collect_atoms(b: &Rc<Bdd>, out: &mut Vec<Atom>) {
    if let Bdd::If { atom, hi, lo } = b.as_ref() {
        if !out.contains(atom) {
            out.push(atom.clone());
        }
        collect_atoms(hi, out);
        collect_atoms(lo, out);
    }
}

/// A finite set of values that *separates* the atoms in `atoms`: for the base
/// fragment, any value's truth pattern over those atoms is matched by one of
/// these representatives (each kind's representatives cover every mentioned
/// singleton plus one "other"). Emptiness over the fragment is therefore
/// decided exactly by testing this set.
fn separating_values(atoms: &[Atom]) -> Vec<Json> {
    let mut vs = vec![
        Json::Null,
        Json::Boolean(true),
        Json::Boolean(false),
        Json::Array(vec![]),
        Json::Object(vec![]),
    ];
    // Numbers: each mentioned singleton, plus one value distinct from all.
    let mut nums: Vec<f64> = atoms
        .iter()
        .filter_map(|a| match a {
            Atom::Number(Some(bits)) => Some(f64::from_bits(*bits)),
            _ => None,
        })
        .collect();
    let fresh_num = nums.iter().cloned().fold(0.0f64, |m, n| m.max(n) + 1.0);
    nums.push(fresh_num);
    vs.extend(nums.into_iter().map(Json::Number));
    // Strings: each mentioned singleton, plus one string distinct from all.
    let mut strs: Vec<String> = atoms
        .iter()
        .filter_map(|a| match a {
            Atom::String(Some(s)) => Some(s.clone()),
            _ => None,
        })
        .collect();
    let fresh_str = format!("{}~", strs.concat());
    strs.push(fresh_str);
    vs.extend(strs.into_iter().map(Json::String));
    vs
}

impl Bdd {
    /// Is the type empty (⊥) — inhabited by no value? Exact over the base-type
    /// fragment. This is the operation that makes subtyping decidable.
    pub fn is_empty(self: &Rc<Bdd>) -> bool {
        if matches!(self.as_ref(), Bdd::Leaf(false)) {
            return true;
        }
        let mut atoms = Vec::new();
        collect_atoms(self, &mut atoms);
        !separating_values(&atoms).iter().any(|v| self.holds(v))
    }
}

/// Subtyping via emptiness: `a` ≤ `b` iff `a \ b` (a ∧ ¬b) is empty.
pub fn subtype(a: &Rc<Bdd>, b: &Rc<Bdd>) -> bool {
    and(a, &not(b)).is_empty()
}

/// Translate the base-type fragment of a `Shape` into a BDD, or `None` if the
/// shape uses a construct outside that fragment (containers with element types,
/// tuples, tvars, arrows). Arrays/objects map to their coarse kind atoms, so a
/// shape is in the fragment only if it does not constrain their contents.
pub fn from_shape(s: &Shape) -> Option<Rc<Bdd>> {
    Some(match s {
        Shape::Blob => Bdd::top(),
        Shape::Null => Bdd::atom(Atom::Null),
        Shape::Bool(b) => Bdd::atom(Atom::Bool(*b)),
        Shape::Number(n) => Bdd::atom(match n {
            Some(x) => Atom::number(*x),
            None => Atom::Number(None),
        }),
        Shape::String(x) => Bdd::atom(Atom::String(x.clone())),
        Shape::Union(a, b) => or(&from_shape(a)?, &from_shape(b)?),
        Shape::Intersection(a, b) => and(&from_shape(a)?, &from_shape(b)?),
        Shape::Neg(a) => not(&from_shape(a)?),
        // `Mismatch(a, b)` denotes `a` minus `b` (the part of a not in b).
        Shape::Mismatch(a, b) => and(&from_shape(a)?, &not(&from_shape(b)?)),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // A tiny xorshift RNG so the tests need no dependencies.
    struct Rng(u64);
    impl Rng {
        fn next(&mut self) -> u64 {
            let mut x = self.0;
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            self.0 = x;
            x
        }
        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }
    }

    fn atom_pool() -> Vec<Atom> {
        vec![
            Atom::Null,
            Atom::Bool(None),
            Atom::Bool(Some(true)),
            Atom::Bool(Some(false)),
            Atom::Number(None),
            Atom::number(0.0),
            Atom::number(1.0),
            Atom::String(None),
            Atom::String(Some("a".into())),
            Atom::Array,
            Atom::Object,
        ]
    }

    fn value_pool() -> Vec<Json> {
        vec![
            Json::Null,
            Json::Boolean(true),
            Json::Boolean(false),
            Json::Number(0.0),
            Json::Number(1.0),
            Json::Number(2.0),
            Json::String("a".into()),
            Json::String("b".into()),
            Json::Array(vec![]),
            Json::Object(vec![]),
        ]
    }

    fn gen_bdd(rng: &mut Rng, depth: usize, atoms: &[Atom]) -> Rc<Bdd> {
        if depth == 0 {
            return if rng.below(2) == 0 {
                Bdd::top()
            } else {
                Bdd::bottom()
            };
        }
        match rng.below(5) {
            0 => Bdd::atom(atoms[rng.below(atoms.len() as u64) as usize].clone()),
            1 => not(&gen_bdd(rng, depth - 1, atoms)),
            2 => or(
                &gen_bdd(rng, depth - 1, atoms),
                &gen_bdd(rng, depth - 1, atoms),
            ),
            3 => and(
                &gen_bdd(rng, depth - 1, atoms),
                &gen_bdd(rng, depth - 1, atoms),
            ),
            _ => Bdd::atom(atoms[rng.below(atoms.len() as u64) as usize].clone()),
        }
    }

    #[test]
    fn boolean_algebra_on_denotation() {
        // For random BDDs and values, `holds` respects not/or/and, and the
        // reduced form is canonical (structurally distinct BDDs never agree on
        // every value unless equal).
        let atoms = atom_pool();
        let values = value_pool();
        let mut rng = Rng(0x1234_5678_9abc_def0);
        for _ in 0..20000 {
            let a = gen_bdd(&mut rng, 4, &atoms);
            let b = gen_bdd(&mut rng, 4, &atoms);
            let na = not(&a);
            let ab_or = or(&a, &b);
            let ab_and = and(&a, &b);
            for v in &values {
                assert_eq!(na.holds(v), !a.holds(v));
                assert_eq!(ab_or.holds(v), a.holds(v) || b.holds(v));
                assert_eq!(ab_and.holds(v), a.holds(v) && b.holds(v));
            }
            // Idempotence / complement laws, structurally (canonical form).
            assert_eq!(or(&a, &a), a);
            assert_eq!(and(&a, &a), a);
            assert_eq!(or(&a, &na), Bdd::top());
            assert_eq!(and(&a, &na), Bdd::bottom());
        }
    }

    /// A large value set covering every distinction the test atoms can make,
    /// used as an independent ground truth for emptiness/subtyping.
    fn big_sample() -> Vec<Json> {
        let mut v = vec![
            Json::Null,
            Json::Boolean(true),
            Json::Boolean(false),
            Json::Array(vec![]),
            Json::Object(vec![]),
        ];
        for n in [-1.0, 0.0, 1.0, 2.0, 3.0, 100.0] {
            v.push(Json::Number(n));
        }
        for s in ["", "a", "b", "c"] {
            v.push(Json::String(s.into()));
        }
        v
    }

    #[test]
    fn is_empty_matches_bruteforce() {
        // Semantic emptiness (which accounts for atom interdependencies the BDD
        // structure does not, e.g. Number(1) ∧ ¬Number) matches a brute-force
        // scan over a complete value sample.
        let atoms = atom_pool();
        let sample = big_sample();
        let mut rng = Rng(0xdead_beef_cafe_babe);
        for _ in 0..20000 {
            let a = gen_bdd(&mut rng, 4, &atoms);
            let empty = a.is_empty();
            let brute = !sample.iter().any(|v| a.holds(v));
            assert_eq!(empty, brute, "is_empty mismatch");
        }
        // A structurally-nontrivial but semantically-empty type is caught.
        let one = Bdd::atom(Atom::number(1.0));
        let any_num = Bdd::atom(Atom::Number(None));
        assert!(and(&one, &not(&any_num)).is_empty()); // 1 ∧ ¬number = ⊥
        assert!(!one.is_empty());
    }

    #[test]
    fn subtype_matches_bruteforce() {
        // `subtype` agrees with "every value of a is a value of b".
        let atoms = atom_pool();
        let sample = big_sample();
        let mut rng = Rng(0x0123_4567_89ab_cdef);
        for _ in 0..20000 {
            let a = gen_bdd(&mut rng, 3, &atoms);
            let b = gen_bdd(&mut rng, 3, &atoms);
            let sub = subtype(&a, &b);
            let brute = sample
                .iter()
                .all(|v| !a.holds(v) || b.holds(v));
            assert_eq!(sub, brute, "subtype mismatch");
        }
        // Singleton ≤ its kind ≤ ⊤; the reverse fails.
        let one = Bdd::atom(Atom::number(1.0));
        let num = Bdd::atom(Atom::Number(None));
        assert!(subtype(&one, &num));
        assert!(subtype(&num, &Bdd::top()));
        assert!(!subtype(&num, &one));
    }

    #[test]
    fn from_shape_matches_check() {
        // On the base fragment, `holds` agrees with `Shape::check` membership.
        let shapes = vec![
            Shape::Null,
            Shape::Bool(None),
            Shape::Bool(Some(true)),
            Shape::Number(None),
            Shape::Number(Some(1.0)),
            Shape::String(None),
            Shape::String(Some("a".into())),
            Shape::Union(Box::new(Shape::Null), Box::new(Shape::Number(None))),
            Shape::Intersection(
                Box::new(Shape::Number(None)),
                Box::new(Shape::Neg(Box::new(Shape::Number(Some(1.0))))),
            ),
            Shape::Neg(Box::new(Shape::Bool(Some(false)))),
        ];
        let values = value_pool();
        for s in &shapes {
            let bdd = from_shape(s).expect("base fragment");
            for v in &values {
                let in_type = s.check(v.clone(), vec![]).is_none();
                assert_eq!(
                    bdd.holds(v),
                    in_type,
                    "shape {s:?} value {v:?}: bdd={} check={in_type}",
                    bdd.holds(v)
                );
            }
        }
    }
}
