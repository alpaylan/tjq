#![allow(unused_variables)]
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter, Json, UnOp};

use crate::inference::TypeInference;
use crate::{Field, Row, Shape, Subtyping};

/// Constraint-based type inference algorithm.
/// Uses constraint generation and solving for type inference.

/// Budget for per-variable possibility lists. Beyond it the variable is
/// widened to ⊤ (Blob) — a sound over-approximation that stops the
/// exponential list growth deeply nested conditionals otherwise cause.
const MAX_POSSIBILITIES_PER_VAR: usize = 64;

fn push_possibility(map: &mut HashMap<usize, Vec<Shape>>, var: usize, shape: Shape) {
    let entry = map.entry(var).or_default();
    if entry.len() >= MAX_POSSIBILITIES_PER_VAR {
        if entry.as_slice() != [Shape::Blob] {
            *entry = vec![Shape::Blob];
        }
        return;
    }
    if !entry.contains(&shape) {
        entry.push(shape);
    }
}

/// Hard ceiling on substituted shape size; beyond this the variable is
/// left unresolved rather than allowed to grow without bound.
const MAX_SHAPE_SIZE: usize = 4096;

pub struct ConstraintInference;

impl TypeInference for ConstraintInference {
    fn infer(&self, filter: &Filter, filters: &HashMap<String, Filter>) -> (Shape, Vec<Shape>) {
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(filter, &mut context, i, o, filters);

        let result = solve(constraints, &context).unwrap_or_default();

        let tin = result.get(i);
        let tout = result.get(o);
        (tin, vec![tout])
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Comparison {
    GreaterThan,
    LessThan,
}

impl From<Comparison> for Ordering {
    fn from(comp: Comparison) -> Self {
        match comp {
            Comparison::GreaterThan => Ordering::Greater,
            Comparison::LessThan => Ordering::Less,
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Comparison::GreaterThan => write!(f, ">"),
            Comparison::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Equality {
    Equal,
    NotEqual,
}

impl Display for Equality {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Equality::Equal => write!(f, "=="),
            Equality::NotEqual => write!(f, "!="),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Relation {
    Subtyping(Subtyping),
    Comparison(Comparison),
    Equality(Equality),
}

impl Display for Relation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Relation::Subtyping(sub) => write!(f, "{}", sub),
            Relation::Comparison(comp) => write!(f, "{}", comp),
            Relation::Equality(eq) => write!(f, "{}", eq),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Rel {
        t1: Shape,
        rel: Relation,
        t2: Shape,
    },
    Conditional {
        c1: Box<Constraint>,
        c2: Box<Constraint>,
    },
    Or(Vec<Constraint>),
    And(Vec<Constraint>),
    False,
}

impl Constraint {
    pub fn check(&self, equalities: &HashMap<usize, Shape>) -> bool {
        match self {
            Constraint::Rel { t1, rel, t2 } => {
                let t1 = t1.replace_tvars(equalities);
                let t2 = t2.replace_tvars(equalities);
                match rel {
                    Relation::Subtyping(subtyping) => t1.subtype(&t2) == *subtyping,
                    Relation::Comparison(comparison) => t1
                        .partial_cmp(&t2)
                        .map(|o| o == (*comparison).into())
                        .unwrap_or(true),
                    Relation::Equality(equality) => (t1 == t2) == (*equality == Equality::Equal),
                }
            }
            Constraint::Conditional { c1, c2 } => !c1.check(equalities) || c2.check(equalities),
            Constraint::Or(cs) => cs.iter().any(|c| c.check(equalities)),
            Constraint::And(cs) => cs.iter().all(|c| c.check(equalities)),
            Constraint::False => false,
        }
    }
}

impl Constraint {
    pub fn not(self) -> Constraint {
        match self {
            Constraint::Rel { t1, rel, t2 } => match rel {
                Relation::Equality(Equality::Equal) => Constraint::Rel {
                    t1,
                    rel: Relation::Equality(Equality::NotEqual),
                    t2,
                },
                Relation::Equality(Equality::NotEqual) => Constraint::Rel {
                    t1,
                    rel: Relation::Equality(Equality::Equal),
                    t2,
                },
                Relation::Comparison(Comparison::LessThan) => Constraint::Or(vec![
                    Constraint::Rel {
                        t1: t1.clone(),
                        rel: Relation::Equality(Equality::Equal),
                        t2: t2.clone(),
                    },
                    Constraint::Rel {
                        t1,
                        rel: Relation::Comparison(Comparison::GreaterThan),
                        t2,
                    },
                ]),
                Relation::Comparison(Comparison::GreaterThan) => Constraint::Or(vec![
                    Constraint::Rel {
                        t1: t1.clone(),
                        rel: Relation::Equality(Equality::Equal),
                        t2: t2.clone(),
                    },
                    Constraint::Rel {
                        t1,
                        rel: Relation::Comparison(Comparison::LessThan),
                        t2,
                    },
                ]),
                Relation::Subtyping(subtyping) => match subtyping {
                    // If A is not a subtype of B, then A is not equal to B, and A is a supertype of B, or A and B are incompatible.
                    Subtyping::Subtype => Constraint::And(vec![
                        Constraint::Rel {
                            t1: t1.clone(),
                            rel: Relation::Equality(Equality::NotEqual),
                            t2: t2.clone(),
                        },
                        Constraint::Or(vec![
                            Constraint::Rel {
                                t1: t1.clone(),
                                rel: Relation::Subtyping(Subtyping::Supertype),
                                t2: t2.clone(),
                            },
                            Constraint::Rel {
                                t1,
                                rel: Relation::Subtyping(Subtyping::Incompatible),
                                t2,
                            },
                        ]),
                    ]),
                    Subtyping::Supertype => Constraint::And(vec![
                        Constraint::Rel {
                            t1: t1.clone(),
                            rel: Relation::Equality(Equality::NotEqual),
                            t2: t2.clone(),
                        },
                        Constraint::Or(vec![
                            Constraint::Rel {
                                t1: t1.clone(),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: t2.clone(),
                            },
                            Constraint::Rel {
                                t1,
                                rel: Relation::Subtyping(Subtyping::Incompatible),
                                t2,
                            },
                        ]),
                    ]),
                    // If they are not incompatible, then they are either subtypes or supertypes of each other.
                    Subtyping::Incompatible => Constraint::Or(vec![
                        Constraint::Rel {
                            t1: t1.clone(),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: t2.clone(),
                        },
                        Constraint::Rel {
                            t1,
                            rel: Relation::Subtyping(Subtyping::Supertype),
                            t2,
                        },
                    ]),
                },
            },
            Constraint::Conditional { c1, c2 } => Constraint::Conditional {
                c1: Box::new(c1.not()),
                c2: Box::new(c2.not()),
            },
            Constraint::Or(cs) => Constraint::And(cs.into_iter().map(|c| c.not()).collect()),
            Constraint::And(cs) => Constraint::Or(cs.into_iter().map(|c| c.not()).collect()),
            Constraint::False => Constraint::False,
        }
    }

    pub fn replace_tvars(&mut self, equalities: &HashMap<usize, Shape>) {
        match self {
            Constraint::Rel { t1, t2, .. } => {
                *t1 = t1.replace_tvars(equalities);
                *t2 = t2.replace_tvars(equalities);
            }
            Constraint::Conditional { c1, c2 } => {
                c1.replace_tvars(equalities);
                c2.replace_tvars(equalities);
            }
            Constraint::Or(cs) | Constraint::And(cs) => {
                for c in cs {
                    c.replace_tvars(equalities);
                }
            }
            Constraint::False => {}
        }
    }
}

impl Shape {
    pub fn replace_tvars(&self, equalities: &HashMap<usize, Shape>) -> Shape {
        match self {
            Shape::TVar(var) => equalities.get(var).cloned().unwrap_or(Shape::TVar(*var)),
            Shape::Object(fields) => Shape::Object(Row {
                fields: fields
                    .iter()
                    .map(|f| Field {
                        key: f.key.clone(),
                        value: f.value.replace_tvars(equalities),
                        optional: f.optional,
                    })
                    .collect(),
                open: fields.open,
            }),
            Shape::Array(elem, size) => {
                Shape::Array(Box::new(elem.replace_tvars(equalities)), *size)
            }
            Shape::Tuple(elems) => {
                Shape::Tuple(elems.iter().map(|e| e.replace_tvars(equalities)).collect())
            }
            Shape::Union(left, right) => Shape::Union(
                Box::new(left.replace_tvars(equalities)),
                Box::new(right.replace_tvars(equalities)),
            ),
            Shape::Intersection(left, right) => Shape::Intersection(
                Box::new(left.replace_tvars(equalities)),
                Box::new(right.replace_tvars(equalities)),
            ),
            Shape::Neg(inner) => Shape::Neg(Box::new(inner.replace_tvars(equalities))),
            _ => self.clone(),
        }
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Rel { t1, rel, t2 } => write!(f, "{} {} {}", t1, rel, t2),
            Constraint::Conditional { c1, c2 } => {
                write!(f, "{} ==> {}", c1, c2)
            }
            Constraint::Or(cs) => {
                write!(f, "(")?;
                for (i, c) in cs.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", c)?;
                }
                write!(f, ")")
            }
            Constraint::And(constraints) => {
                write!(f, "(")?;
                for (i, c) in constraints.iter().enumerate() {
                    if i != 0 {
                        write!(f, " & ")?;
                    }
                    write!(f, "{}", c)?;
                }
                write!(f, ")")
            }
            Constraint::False => {
                write!(f, "error")
            }
        }
    }
}

impl Constraint {
    pub fn dependencies(&self) -> Vec<usize> {
        match self {
            Constraint::Rel { t1, t2, .. } => {
                let mut deps = t1.dependencies();
                deps.extend(t2.dependencies());
                deps
            }
            Constraint::Conditional { c1, c2 } => {
                let mut deps = c1.dependencies();
                deps.extend(c2.dependencies());
                deps
            }
            Constraint::Or(cs) => cs.iter().flat_map(|c| c.dependencies()).collect(),
            Constraint::And(cs) => cs.iter().flat_map(|c| c.dependencies()).collect(),
            Constraint::False => vec![],
        }
    }
}
type Constraints = Vec<Constraint>;

#[derive(Debug, Clone)]
pub struct Facts {
    pub var: usize,
    pub equalities: Vec<Shape>,
    pub inequalities: Vec<Shape>,
    pub subtypes: Vec<Shape>,
    pub supertypes: Vec<Shape>,
    pub incompatibilies: Vec<Shape>,
}

impl Facts {
    pub fn new(var: usize) -> Self {
        Facts {
            var,
            equalities: vec![],
            inequalities: vec![],
            subtypes: vec![],
            supertypes: vec![],
            incompatibilies: vec![],
        }
    }

    pub fn extend(&mut self, other: Facts) {
        self.equalities.extend(other.equalities);
        self.inequalities.extend(other.inequalities);
        self.subtypes.extend(other.subtypes);
        self.supertypes.extend(other.supertypes);
        self.incompatibilies.extend(other.incompatibilies);
        self.cleanup();
    }

    fn cleanup(&mut self) {
        // Remove equalities to larger variables
        self.equalities.retain(|e| match e {
            Shape::TVar(v) => *v < self.var,
            _ => true,
        });
    }
}

#[derive(Debug)]
pub struct TypeEnv {
    pub facts: HashMap<usize, Facts>,
    pub implications: Vec<(Constraint, Constraint)>,
    pub possibilities: Vec<Vec<Constraint>>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            facts: HashMap::new(),
            implications: vec![],
            possibilities: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
}

/// Result of solving constraints - maps type variables to their resolved shapes
#[derive(Debug, Clone)]
pub struct SolverResult {
    /// Resolved types for each type variable
    pub resolved: HashMap<usize, Shape>,
    /// Type variables that couldn't be fully resolved
    pub unresolved: HashSet<usize>,
    /// Any type errors encountered
    pub errors: Vec<TypeError>,
    /// Lint findings (docs/type-system-scope.md §5); the program is still
    /// well-typed, but something is provably suspicious.
    pub warnings: Vec<TypeWarning>,
}

/// A lint-level diagnostic: the `dead-condition-branch` rule fires when a
/// conditional's condition type is provably always-truthy (dead else) or
/// always-falsy (dead then).
#[derive(Debug, Clone, PartialEq)]
pub struct TypeWarning {
    pub message: String,
}

impl Default for SolverResult {
    fn default() -> Self {
        Self::new()
    }
}

impl SolverResult {
    pub fn new() -> Self {
        SolverResult {
            resolved: HashMap::new(),
            unresolved: HashSet::new(),
            errors: vec![],
            warnings: vec![],
        }
    }

    /// Get the resolved type for a type variable, or TVar if unresolved
    pub fn get(&self, var: usize) -> Shape {
        self.resolved.get(&var).cloned().unwrap_or(Shape::TVar(var))
    }
}

pub fn solve(mut constraints: Vec<Constraint>, ctx: &Context) -> Result<SolverResult, TypeError> {
    let mut env = TypeEnv::new();
    let mut result = SolverResult::new();
    result.warnings.extend(ctx.warnings.iter().cloned());

    let mut facts = vec![];
    for i in 0..=ctx.vars {
        facts.push(Facts::new(i));
    }

    // Phase 1: Collect all constraints into facts
    while let Some(c) = constraints.pop() {
        match c {
            Constraint::Rel { t1, rel, t2 } => match rel {
                Relation::Equality(equality) => match (t1, t2) {
                    (Shape::TVar(t1), Shape::TVar(t2)) => match equality {
                        Equality::Equal => {
                            facts[t1].equalities.push(Shape::TVar(t2));
                            facts[t2].equalities.push(Shape::TVar(t1));
                        }
                        Equality::NotEqual => {
                            facts[t1].inequalities.push(Shape::TVar(t2));
                            facts[t2].inequalities.push(Shape::TVar(t1));
                        }
                    },
                    (Shape::TVar(t), t_) | (t_, Shape::TVar(t)) => match equality {
                        Equality::Equal => facts[t].equalities.push(t_.clone()),
                        Equality::NotEqual => facts[t].inequalities.push(t_.clone()),
                    },
                    (t1, t2) => {
                        // Two concrete types - check if they are equal
                        match equality {
                            Equality::Equal => {
                                if t1 != t2 {
                                    result.errors.push(TypeError {
                                        message: format!("Type mismatch: {} != {}", t1, t2),
                                    });
                                }
                            }
                            Equality::NotEqual => {
                                if t1 == t2 {
                                    result.errors.push(TypeError {
                                        message: format!(
                                            "Types should be different but are equal: {} == {}",
                                            t1, t2
                                        ),
                                    });
                                }
                            }
                        }
                    }
                },
                Relation::Subtyping(rel) => match (t1, t2) {
                    (Shape::TVar(t1), Shape::TVar(t2)) => match rel {
                        Subtyping::Subtype => {
                            facts[t1].subtypes.push(Shape::TVar(t2));
                            facts[t2].supertypes.push(Shape::TVar(t1));
                        }
                        Subtyping::Supertype => {
                            facts[t1].supertypes.push(Shape::TVar(t2));
                            facts[t2].subtypes.push(Shape::TVar(t1));
                        }
                        Subtyping::Incompatible => {
                            facts[t1].incompatibilies.push(Shape::TVar(t2));
                            facts[t2].incompatibilies.push(Shape::TVar(t1));
                        }
                    },
                    (Shape::TVar(t), t_) => match rel {
                        Subtyping::Subtype => {
                            facts[t].subtypes.push(t_.clone());
                        }
                        Subtyping::Supertype => {
                            facts[t].supertypes.push(t_.clone());
                        }
                        Subtyping::Incompatible => {
                            facts[t].incompatibilies.push(t_.clone());
                        }
                    },
                    (t_, Shape::TVar(t)) => match rel {
                        Subtyping::Subtype => {
                            facts[t].supertypes.push(t_.clone());
                        }
                        Subtyping::Supertype => {
                            facts[t].subtypes.push(t_.clone());
                        }
                        Subtyping::Incompatible => {
                            facts[t].incompatibilies.push(t_.clone());
                        }
                    },
                    (t1, t2) => {
                        // Two concrete types - verify subtyping relation
                        let actual_rel = t1.subtype(&t2);
                        if actual_rel != rel {
                            result.errors.push(TypeError {
                                message: format!(
                                    "Subtyping violation: {} {} {} (expected {})",
                                    t1, actual_rel, t2, rel
                                ),
                            });
                        }
                    }
                },
                Relation::Comparison(comparison) => {
                    // For comparisons between concrete types, verify the relation
                    match (&t1, &t2) {
                        (Shape::TVar(_), _) | (_, Shape::TVar(_)) => {
                            // Can't evaluate comparison with type variables yet
                            // Store as implication or possibility
                        }
                        _ => {
                            if let Some(ord) = t1.partial_cmp(&t2) {
                                let expected: Ordering = comparison.into();
                                if ord != expected {
                                    result.errors.push(TypeError {
                                        message: format!(
                                            "Comparison failed: {} {:?} {} (expected {:?})",
                                            t1, ord, t2, expected
                                        ),
                                    });
                                }
                            }
                        }
                    }
                }
            },

            Constraint::Conditional { c1, c2 } => env.implications.push((*c1, *c2)),
            Constraint::Or(cs) => {
                env.possibilities.push(cs);
            }
            Constraint::And(cs) => {
                for c in cs {
                    constraints.push(c);
                }
            }
            Constraint::False => {
                result.errors.push(TypeError {
                    message: "Unsatisfiable constraint (False)".to_string(),
                });
            }
        }
    }

    env.facts = facts.into_iter().enumerate().collect();

    // Phase 2: Build substitution map from equalities
    // Use union-find style approach: map each variable to its canonical representative
    let mut substitutions: HashMap<usize, Shape> = HashMap::new();

    // Pass 1: variable-to-variable aliases only — map each variable to a
    // smaller one in its equality class.
    for (var, fact) in env.facts.iter() {
        for eq in &fact.equalities {
            if let Shape::TVar(other_var) = eq {
                if *other_var < *var {
                    substitutions.insert(*var, Shape::TVar(*other_var));
                }
            }
        }
    }
    // Close the aliases so every variable maps to its class representative
    // (the smallest member) before concretes are attached.
    {
        let mut changed = true;
        let mut rounds = 0;
        while changed && rounds < 50 {
            rounds += 1;
            changed = false;
            let keys: Vec<usize> = substitutions.keys().copied().collect();
            for var in keys {
                let cur = substitutions.get(&var).unwrap().clone();
                let new = cur.replace_tvars(&substitutions);
                if new != cur {
                    substitutions.insert(var, new);
                    changed = true;
                }
            }
        }
    }
    // Pass 2: attach each concrete equality to its class representative, not
    // just the variable that carried it. Previously a competing smaller-var
    // alias could overwrite the concrete (so `left == null` was lost when
    // `left == input` followed it, leaving the whole class unresolved).
    for (var, fact) in env.facts.iter() {
        for eq in &fact.equalities {
            if !matches!(eq, Shape::TVar(_)) {
                let root = match substitutions.get(var) {
                    Some(Shape::TVar(r)) => *r,
                    _ => *var,
                };
                substitutions.insert(root, eq.clone());
            }
        }
    }

    // Transitively close the substitutions
    // Bounded: cyclic equalities (T == Union(T, x)) would otherwise grow
    // the substituted shape forever and overflow the stack.
    let mut changed = true;
    let mut closure_rounds = 0;
    while changed && closure_rounds < 50 {
        closure_rounds += 1;
        changed = false;
        let keys: Vec<usize> = substitutions.keys().copied().collect();
        for var in keys {
            let current = substitutions.get(&var).unwrap().clone();
            if current.dependencies().contains(&var) {
                // Self-referential substitution: drop it, the variable
                // cannot be eliminated.
                substitutions.remove(&var);
                changed = true;
                continue;
            }
            let new_val = current.replace_tvars(&substitutions);
            if new_val != current {
                if new_val.size() > MAX_SHAPE_SIZE {
                    // Substitution is blowing up (cyclic aliasing with
                    // multiple occurrences); the variable cannot be
                    // eliminated within budget.
                    substitutions.remove(&var);
                } else {
                    substitutions.insert(var, new_val);
                }
                changed = true;
            }
        }
    }

    // Phase 3: Apply substitutions to all facts
    for (_, fact) in env.facts.iter_mut() {
        fact.equalities = fact
            .equalities
            .iter()
            .map(|t| t.replace_tvars(&substitutions))
            .collect();
        fact.inequalities = fact
            .inequalities
            .iter()
            .map(|t| t.replace_tvars(&substitutions))
            .collect();
        fact.subtypes = fact
            .subtypes
            .iter()
            .map(|t| t.replace_tvars(&substitutions))
            .collect();
        fact.supertypes = fact
            .supertypes
            .iter()
            .map(|t| t.replace_tvars(&substitutions))
            .collect();
        fact.incompatibilies = fact
            .incompatibilies
            .iter()
            .map(|t| t.replace_tvars(&substitutions))
            .collect();
    }

    // Merge the facts of variables substituted by another variable into their
    // canonical representative, so bounds recorded on the substituted variable
    // (e.g. `T3 <: number` when T3 == T1) are not lost when Phase 4 skips it.
    let substituted_vars: Vec<usize> = substitutions.keys().copied().collect();
    for var in substituted_vars {
        if let Some(Shape::TVar(canonical)) = substitutions.get(&var) {
            let canonical = *canonical;
            if let Some(fact) = env.facts.get(&var).cloned() {
                if let Some(canonical_fact) = env.facts.get_mut(&canonical) {
                    canonical_fact.extend(fact);
                }
            }
        }
    }

    // Phase 4: Resolve each variable's type from its facts
    for (var, fact) in env.facts.iter() {
        // Skip variables that have been substituted away
        if substitutions.contains_key(var) {
            continue;
        }

        // Try to find a concrete type from equalities
        let concrete_eq = fact
            .equalities
            .iter()
            .find(|t| !matches!(t, Shape::TVar(_)));

        if let Some(concrete) = concrete_eq {
            result.resolved.insert(*var, concrete.clone());
            continue;
        }

        // Try to compute type from subtyping constraints
        // If we have subtypes, we need a type that is a supertype of all of them (join/LUB)
        // If we have supertypes, we need a type that is a subtype of all of them (meet/GLB)
        let resolved_type = resolve_from_bounds(fact, &substitutions);

        match resolved_type {
            Some(t) => {
                result.resolved.insert(*var, t);
            }
            None => {
                // Couldn't resolve - might still be polymorphic
                result.unresolved.insert(*var);
            }
        }
    }

    // Add substituted variables to resolved
    for (var, shape) in &substitutions {
        let final_shape = shape.replace_tvars(&result.resolved);
        result.resolved.insert(*var, final_shape);
    }

    // Phase 4b: Transitively close resolved types
    // Some resolved types may contain TVars that were resolved later
    let mut changed = true;
    let mut closure_rounds = 0;
    while changed && closure_rounds < 50 {
        closure_rounds += 1;
        changed = false;
        let vars: Vec<usize> = result.resolved.keys().copied().collect();
        for var in vars {
            let current = result.resolved.get(&var).unwrap().clone();
            if current.dependencies().contains(&var) {
                // Self-referential resolution: the variable stays unresolved
                // rather than growing without bound.
                result.resolved.remove(&var);
                result.unresolved.insert(var);
                changed = true;
                continue;
            }
            let new_val = current.replace_tvars(&result.resolved);
            if new_val != current {
                if new_val.size() > MAX_SHAPE_SIZE {
                    result.resolved.remove(&var);
                    result.unresolved.insert(var);
                } else {
                    result.resolved.insert(var, new_val);
                }
                changed = true;
            }
        }
    }

    // Phase 5: Handle error-forcing implications
    // If we have `condition ==> False`, then `condition` must be false for the program to be well-typed
    // This allows us to infer types from the negation of the condition
    let mut forced_true: Vec<Constraint> = vec![];
    let mut forced_false: Vec<Constraint> = vec![];

    for (condition, consequence) in &env.implications {
        // Check if consequence is False or contains False
        if consequence_is_false(consequence) {
            // The condition must be false for the program to be well-typed
            forced_false.push(condition.clone());

            // If condition is `TVar(x) == value`, then x != value
            // If condition is `TVar(x) == false`, then x must be true (for booleans)
            if let Constraint::Rel {
                t1: Shape::TVar(var),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Bool(Some(false)),
            } = condition
            {
                // var must be true (since it's a boolean and can't be false)
                // Allow overwriting Bool(None) with Bool(Some(true)) for more precise typing
                let should_update = match result.resolved.get(var) {
                    None => true,
                    Some(Shape::Bool(None)) => true, // Refine generic bool to specific true
                    _ => false,
                };
                if should_update {
                    result.resolved.insert(*var, Shape::Bool(Some(true)));
                    result.unresolved.remove(var);
                }
                // Mark the negation as forced true
                forced_true.push(Constraint::Rel {
                    t1: Shape::TVar(*var),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(true)),
                });
            }

            // If condition is `TVar(x) == true`, then x must be false (for booleans)
            // This handles cases like: if isarray | not then error else ... end
            // where error is in the then-branch
            if let Constraint::Rel {
                t1: Shape::TVar(var),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Bool(Some(true)),
            } = condition
            {
                // var must be false (since it's a boolean and can't be true)
                let should_update = match result.resolved.get(var) {
                    None => true,
                    Some(Shape::Bool(None)) => true, // Refine generic bool to specific false
                    _ => false,
                };
                if should_update {
                    result.resolved.insert(*var, Shape::Bool(Some(false)));
                    result.unresolved.remove(var);
                }
                // Mark the negation as forced true
                forced_true.push(Constraint::Rel {
                    t1: Shape::TVar(*var),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(false)),
                });
            }
        }
    }

    // Phase 5b: Backward propagation through conditional constraints
    // When we force a variable to a value, trace back through conditionals to find
    // what input values would produce that output
    backward_propagate_constraints(
        &mut result,
        &mut forced_true,
        &env.implications,
        &substitutions,
    );

    // Process implications where the condition matches a forced_true constraint
    // For disjunctions, trace through each branch and collect possible values
    let mut var_possibilities: HashMap<usize, Vec<Shape>> = HashMap::new();

    // Helper: trace implications starting from an assumed constraint
    fn trace_implications(
        assumption: &Constraint,
        implications: &[(Constraint, Constraint)],
        var_possibilities: &mut HashMap<usize, Vec<Shape>>,
        substitutions: &HashMap<usize, Shape>,
    ) {
        for (condition, consequence) in implications {
            if constraints_match(condition, assumption) {
                // This implication fires under our assumption
                match consequence {
                    Constraint::Rel {
                        t1: Shape::TVar(var),
                        rel: Relation::Equality(Equality::Equal),
                        t2: value,
                    } => {
                        let canonical = get_canonical_var(*var, substitutions);
                        let resolved_val = value.replace_tvars(substitutions);
                        push_possibility(var_possibilities, canonical, resolved_val);
                    }
                    Constraint::Rel {
                        t1: value,
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::TVar(var),
                    } if !matches!(value, Shape::TVar(_)) => {
                        let canonical = get_canonical_var(*var, substitutions);
                        let resolved_val = value.replace_tvars(substitutions);
                        push_possibility(var_possibilities, canonical, resolved_val);
                    }
                    _ => {}
                }
            }
        }
    }

    // Track comparison bounds for type variables
    // Lower bounds: types that must be <= the variable (variable >= lower_bound)
    // Upper bounds: types that must be > the variable (variable < upper_bound)
    let mut lower_bounds: HashMap<usize, Vec<Shape>> = HashMap::new();
    let mut upper_bounds: HashMap<usize, Vec<Shape>> = HashMap::new();

    // Iteratively process implications until no new constraints are derived
    let mut iteration = 0;
    let max_iterations = 100; // Prevent infinite loops
    loop {
        iteration += 1;
        if iteration > max_iterations {
            break;
        }

        let prev_resolved_count = result.resolved.len();
        let prev_bounds_count = lower_bounds.values().map(|v| v.len()).sum::<usize>()
            + upper_bounds.values().map(|v| v.len()).sum::<usize>();
        let prev_possibilities_count = var_possibilities.values().map(|v| v.len()).sum::<usize>();

        // Collect nested implications from activated consequences
        let mut nested_implications: Vec<(Constraint, Constraint)> = Vec::new();

        // Process implications
        for (condition, consequence) in &env.implications {
            let condition_satisfied = forced_true.iter().any(|f| constraints_match(condition, f))
                || condition_is_satisfied(condition, &result.resolved);

            if condition_satisfied {
                // If consequence is a disjunction, trace each branch
                if let Constraint::Or(options) = consequence {
                    for opt in options {
                        if let Constraint::Rel {
                            t1: Shape::TVar(_),
                            rel: Relation::Equality(Equality::Equal),
                            t2: _,
                        } = opt
                        {
                            // This is an assumption we can make - trace its implications
                            trace_implications(
                                opt,
                                &env.implications,
                                &mut var_possibilities,
                                &substitutions,
                            );
                        }
                    }
                    // Also extract comparison bounds from Or options
                    extract_comparison_bounds_from_or(
                        options,
                        &mut lower_bounds,
                        &mut upper_bounds,
                        &substitutions,
                        &result.resolved,
                    );
                } else {
                    // Direct constraint - extract and resolve
                    extract_and_resolve_constraints(consequence, &mut result, &substitutions);
                    // Also extract comparison bounds
                    extract_comparison_bounds(
                        consequence,
                        &mut lower_bounds,
                        &mut upper_bounds,
                        &substitutions,
                        &result.resolved,
                    );
                    // Extract nested implications from the consequence
                    extract_nested_implications(consequence, &mut nested_implications);
                }
            }
        }

        // Group implications by their condition variable to handle both branches.
        // Top-level implications (e.g. from if-then-else) and nested ones are
        // treated uniformly: an undetermined boolean condition means both
        // branches are possible.
        let mut implications_by_var: HashMap<usize, Vec<(bool, &Constraint)>> = HashMap::new();
        for (condition, consequence) in env.implications.iter().chain(nested_implications.iter()) {
            if let Constraint::Rel {
                t1: Shape::TVar(var),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Bool(Some(value)),
            } = condition
            {
                let canonical = get_canonical_var(*var, &substitutions);
                implications_by_var
                    .entry(canonical)
                    .or_default()
                    .push((*value, consequence));
            }
        }

        for (condition, consequence) in &nested_implications {
            let condition_satisfied = forced_true.iter().any(|f| constraints_match(condition, f))
                || condition_is_satisfied(condition, &result.resolved);

            if condition_satisfied {
                extract_and_resolve_constraints(consequence, &mut result, &substitutions);
                extract_comparison_bounds(
                    consequence,
                    &mut lower_bounds,
                    &mut upper_bounds,
                    &substitutions,
                    &result.resolved,
                );
            }
        }

        // For undetermined conditions, process both branches as possibilities
        // This allows output type to be union of both branches
        for (var, branches) in &implications_by_var {
            // Undetermined: neither provably truthy nor provably falsy
            let is_undetermined = match result.resolved.get(var) {
                None => true,
                Some(shape) => {
                    !shape.disjoint_with(&falsy_shape()) && !shape.included_in(&falsy_shape())
                }
            };

            if is_undetermined && branches.len() >= 2 {
                // Both true and false branches exist - extract types from both
                for (_, consequence) in branches {
                    // Extract types as possibilities (using var_possibilities instead of direct resolution)
                    extract_types_as_possibilities(
                        consequence,
                        &mut var_possibilities,
                        &substitutions,
                    );
                }
            }
        }

        // Check if we derived any new constraints
        let new_resolved_count = result.resolved.len();
        let new_bounds_count = lower_bounds.values().map(|v| v.len()).sum::<usize>()
            + upper_bounds.values().map(|v| v.len()).sum::<usize>();
        let new_possibilities_count = var_possibilities.values().map(|v| v.len()).sum::<usize>();

        if new_resolved_count == prev_resolved_count
            && new_bounds_count == prev_bounds_count
            && new_possibilities_count == prev_possibilities_count
        {
            break; // Fixed point reached
        }
    }

    // Now resolve type variables from their comparison bounds
    resolve_from_comparison_bounds(&mut result, &lower_bounds, &upper_bounds, &substitutions);

    // Propagate possibilities through subtype constraints
    // If we have T15 <: T2 and T15 has possibilities, propagate to T2
    propagate_possibilities_through_subtypes(
        &mut var_possibilities,
        &env.implications,
        &result.resolved,
        &substitutions,
    );

    // Add collected possibilities to result
    for (var, types) in var_possibilities {
        if result.resolved.contains_key(&var) || substitutions.contains_key(&var) {
            continue;
        }

        // Remove duplicates
        let mut unique_types: Vec<Shape> = vec![];
        for t in types {
            if !unique_types.iter().any(|existing| existing == &t) {
                unique_types.push(t);
            }
        }

        if unique_types.len() == 1 {
            result
                .resolved
                .insert(var, unique_types.into_iter().next().unwrap());
            result.unresolved.remove(&var);
        } else if unique_types.len() > 1 {
            // Alternative branches: the variable may be any of them, so union
            let union_type = unique_types
                .into_iter()
                .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                .unwrap();
            result.resolved.insert(var, union_type);
            result.unresolved.remove(&var);
        }
    }

    // Canonicalize types (e.g., Union(true, false) -> Bool(None))
    for (var, shape) in result.resolved.iter_mut() {
        *shape = canonicalize_shape(shape);
    }

    // Re-run transitive closure after new resolutions
    let mut changed = true;
    let mut closure_rounds = 0;
    while changed && closure_rounds < 50 {
        closure_rounds += 1;
        changed = false;
        let vars: Vec<usize> = result.resolved.keys().copied().collect();
        for var in vars {
            let current = result.resolved.get(&var).unwrap().clone();
            if current.dependencies().contains(&var) {
                // Self-referential resolution: the variable stays unresolved
                // rather than growing without bound.
                result.resolved.remove(&var);
                result.unresolved.insert(var);
                changed = true;
                continue;
            }
            let new_val = current.replace_tvars(&result.resolved);
            if new_val != current {
                if new_val.size() > MAX_SHAPE_SIZE {
                    result.resolved.remove(&var);
                    result.unresolved.insert(var);
                } else {
                    result.resolved.insert(var, new_val);
                }
                changed = true;
            }
        }
    }

    // Phase 6: Verify remaining implications
    for (condition, consequence) in &env.implications {
        let cond_resolved = {
            let mut c = condition.clone();
            c.replace_tvars(&result.resolved);
            c
        };
        let cons_resolved = {
            let mut c = consequence.clone();
            c.replace_tvars(&result.resolved);
            c
        };

        // If condition is satisfied, consequence must also be satisfied
        if cond_resolved.check(&result.resolved) && !cons_resolved.check(&result.resolved) {
            result.errors.push(TypeError {
                message: format!(
                    "Implication violated: {} ==> {} (condition true but consequence false)",
                    condition, consequence
                ),
            });
        }
    }

    // Phase 6: Handle disjunctions - infer types from satisfiable branches
    // For each unresolved variable, collect possible types from all valid branches
    let mut var_possibilities: HashMap<usize, Vec<Shape>> = HashMap::new();
    // Variables that some satisfiable branch ties to an *unresolved* value
    // (e.g. `T2 == T4` with T4 unknown, from `+`'s null overload). Resolving
    // such a variable from the other branches alone would drop this branch
    // and under-approximate the type, so they stay unresolved instead.
    let mut poisoned_vars: HashSet<usize> = HashSet::new();

    for possibilities in &env.possibilities {
        // Check which branches are satisfiable given current knowledge
        let satisfiable_branches: Vec<&Constraint> = possibilities
            .iter()
            .filter(|p| {
                // A branch is potentially satisfiable if it doesn't contradict known facts
                branch_is_satisfiable(p, &result.resolved, &substitutions)
            })
            .collect();

        if satisfiable_branches.is_empty() {
            // No satisfiable branch found - skip this disjunction
            continue;
        }

        // Extract type constraints from each satisfiable branch
        for branch in &satisfiable_branches {
            let branch_types =
                extract_types_from_branch(branch, &substitutions, &mut poisoned_vars);
            for (var, shape) in branch_types {
                var_possibilities.entry(var).or_default().push(shape);
            }
        }
    }

    // Update resolved types based on disjunction possibilities
    for (var, possible_types) in var_possibilities {
        if result.resolved.contains_key(&var) || substitutions.contains_key(&var) {
            continue; // Already resolved
        }
        if poisoned_vars.contains(&var) {
            continue; // Some branch leaves this variable unknown
        }

        // Remove duplicates (manual approach since Shape doesn't implement Hash)
        let mut unique_types: Vec<Shape> = vec![];
        for t in possible_types {
            if !unique_types.iter().any(|existing| existing == &t) {
                unique_types.push(t);
            }
        }

        if unique_types.len() == 1 {
            result
                .resolved
                .insert(var, unique_types.into_iter().next().unwrap());
            result.unresolved.remove(&var);
        } else if unique_types.len() > 1 {
            // Multiple possibilities - create a union
            let union_type = unique_types
                .into_iter()
                .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                .unwrap();
            result.resolved.insert(var, union_type);
            result.unresolved.remove(&var);
        }
    }

    // Final canonicalization: unions assembled after the mid-solve
    // canonicalization pass (e.g. in the disjunction phase above) still
    // need absorption applied.
    for (_, shape) in result.resolved.iter_mut() {
        *shape = shape.canonicalize();
    }

    // dead-condition-branch lint (docs/type-system-scope.md §5): a
    // conditional whose condition type is provably always-truthy or
    // always-falsy has an unreachable branch. Conditions with both
    // polarities in the implications came from real conditionals;
    // conditions forced by backward propagation (`... else error end`)
    // are the narrowing idiom and stay silent.
    let mut condition_polarity: HashMap<usize, (bool, bool)> = HashMap::new();
    for (condition, _) in &env.implications {
        if let Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Equality(Equality::Equal),
            t2: Shape::Bool(Some(polarity)),
        } = condition
        {
            let canonical = get_canonical_var(*var, &substitutions);
            let entry = condition_polarity
                .entry(canonical)
                .or_insert((false, false));
            if *polarity {
                entry.0 = true;
            } else {
                entry.1 = true;
            }
        }
    }
    for (var, (has_true, has_false)) in condition_polarity {
        if !(has_true && has_false) {
            continue;
        }
        let forced = forced_true.iter().any(|f| {
            matches!(f, Constraint::Rel { t1: Shape::TVar(v), rel: Relation::Equality(Equality::Equal), t2: Shape::Bool(Some(_)) } if get_canonical_var(*v, &substitutions) == var)
        });
        if forced {
            continue;
        }
        let Some(shape) = result.resolved.get(&var) else {
            continue;
        };
        if shape.disjoint_with(&falsy_shape()) {
            result.warnings.push(TypeWarning {
                message: format!(
                    "condition is always truthy (type {shape}); the else branch is unreachable"
                ),
            });
        } else if shape.included_in(&falsy_shape()) {
            result.warnings.push(TypeWarning {
                message: format!(
                    "condition is always falsy (type {shape}); the then branch is unreachable"
                ),
            });
        }
    }

    Ok(result)
}

/// Infer a filter's type as an intersection of arrows (input -> output).
///
/// Overloaded builtins like `+` produce disjunctive constraints, one branch
/// per overload. Solving the full system merges each variable's branch
/// alternatives into a union, which loses the correlation between input and
/// output: `. + .` becomes `(null | number | string) -> (null | number | string)`,
/// even though a string input can only produce a string output. Solving one
/// branch combination at a time keeps that correlation, and the filter's type
/// is the intersection of the per-branch arrows:
///   `. + .` : (number -> number) & (string -> string) & (null -> null)
pub fn solve_arrows(
    constraints: Vec<Constraint>,
    ctx: &Context,
    i: usize,
    o: usize,
) -> Result<Shape, TypeError> {
    // Each top-level disjunction is an overloaded operation; its branches are
    // the overloads.
    let or_positions: Vec<usize> = constraints
        .iter()
        .enumerate()
        .filter_map(|(idx, c)| matches!(c, Constraint::Or(_)).then_some(idx))
        .collect();

    let solve_plain = |constraints: Vec<Constraint>| -> Result<Shape, TypeError> {
        let result = solve(constraints, ctx)?;
        Ok(Shape::arrow(result.get(i), result.get(o)).canonicalize())
    };

    if or_positions.is_empty() {
        return solve_plain(constraints);
    }

    let branch_counts: Vec<usize> = or_positions
        .iter()
        .map(|&p| match &constraints[p] {
            Constraint::Or(branches) => branches.len(),
            _ => unreachable!(),
        })
        .collect();

    // Cap the branch combinations to keep the analysis cheap; beyond the cap,
    // fall back to the union-based whole-system solution.
    const MAX_COMBINATIONS: usize = 64;
    let total: usize = branch_counts.iter().product();
    if total > MAX_COMBINATIONS {
        return solve_plain(constraints);
    }

    let no_substitutions = HashMap::new();
    let mut arrows: Vec<Shape> = vec![];
    for combo in 0..total {
        // Decode the combination index into one branch choice per disjunction
        let mut branch_constraints = constraints.clone();
        let mut selected: Vec<&Constraint> = vec![];
        let mut rem = combo;
        for (&p, &n) in or_positions.iter().zip(&branch_counts) {
            let choice = rem % n;
            rem /= n;
            let Constraint::Or(branches) = &constraints[p] else {
                unreachable!()
            };
            branch_constraints[p] = branches[choice].clone();
            selected.push(&branches[choice]);
        }

        let Ok(result) = solve(branch_constraints, ctx) else {
            continue;
        };
        if !result.errors.is_empty() {
            continue;
        }
        // The solver does not check every fact for consistency; reject
        // combinations whose chosen branches contradict the resolved types
        // (e.g. the string overload of `+` when the input is a number).
        if !selected
            .iter()
            .all(|b| branch_is_satisfiable(b, &result.resolved, &no_substitutions))
        {
            continue;
        }

        let arrow = Shape::arrow(result.get(i), result.get(o)).canonicalize();
        if !arrows.contains(&arrow) {
            arrows.push(arrow);
        }
    }

    if arrows.is_empty() {
        // No branch combination is satisfiable on its own; report whatever the
        // whole-system solution says (including its errors).
        return solve_plain(constraints);
    }

    Ok(Shape::intersection_(arrows).canonicalize())
}

/// Backward propagation through conditional constraints
/// When a variable is forced to a specific value, trace back through conditionals
/// to determine what input values would produce that output.
///
/// For example, if we have:
///   - T3 is forced to false
///   - T11 == true ==> T12 <: T3 with T12 == false
///   - T11 == false ==> T13 <: T3 with T13 == true
/// Then for T3 = false, we need T11 = true.
fn backward_propagate_constraints(
    result: &mut SolverResult,
    forced_true: &mut Vec<Constraint>,
    implications: &[(Constraint, Constraint)],
    substitutions: &HashMap<usize, Shape>,
) {
    let max_iterations = 20;
    let mut iteration = 0;

    loop {
        iteration += 1;
        if iteration > max_iterations {
            break;
        }

        let prev_resolved_count = result.resolved.len();

        // For each resolved boolean variable, try to propagate backward
        let resolved_bools: Vec<(usize, bool)> = result
            .resolved
            .iter()
            .filter_map(|(var, shape)| match shape {
                Shape::Bool(Some(b)) => Some((*var, *b)),
                _ => None,
            })
            .collect();

        for (target_var, target_value) in resolved_bools {
            // Find implications where target_var appears in the consequence as a subtype target
            // Pattern: X == some_value ==> Y <: target_var  with  Y == some_output
            // If target_var = target_value, find which X value produces that Y value

            // Collect all implications that contribute to target_var
            let mut contributors: Vec<(usize, bool, bool)> = vec![]; // (condition_var, condition_value, output_value)

            for (condition, consequence) in implications {
                // Check if consequence is a subtyping constraint to target_var
                if let Constraint::Rel {
                    t1: Shape::TVar(output_var),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::TVar(target),
                } = consequence
                {
                    let canonical_target = get_canonical_var(*target, substitutions);
                    if canonical_target != target_var {
                        continue;
                    }

                    // Found: condition ==> output_var <: target_var
                    // Now find what value output_var has under this condition
                    if let Constraint::Rel {
                        t1: Shape::TVar(cond_var),
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::Bool(Some(cond_value)),
                    } = condition
                    {
                        let canonical_cond = get_canonical_var(*cond_var, substitutions);
                        let canonical_output = get_canonical_var(*output_var, substitutions);

                        // Find if there's an implication: cond_var == cond_value ==> output_var == some_bool
                        for (cond2, conseq2) in implications {
                            if constraints_match(cond2, condition) {
                                // Extract equality constraints from the consequence
                                // It might be a direct Rel or wrapped in And
                                let equalities = extract_equalities_from_constraint(conseq2);
                                for (out_var, out_value) in equalities {
                                    let canonical_out2 = get_canonical_var(out_var, substitutions);
                                    if canonical_out2 == canonical_output {
                                        contributors.push((canonical_cond, *cond_value, out_value));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Now analyze contributors to determine what condition value produces target_value
            // For the `not` pattern:
            // - (cond_var, true, false) means: when cond_var=true, output is false
            // - (cond_var, false, true) means: when cond_var=false, output is true
            // If target_value is false, we need cond_var=true (to get output=false)
            // If target_value is true, we need cond_var=false (to get output=true)

            for (cond_var, cond_value, out_value) in &contributors {
                if *out_value == target_value {
                    // This branch produces the target value
                    // Force the condition variable to cond_value
                    let should_update = match result.resolved.get(cond_var) {
                        None => true,
                        Some(Shape::Bool(None)) => true,
                        _ => false,
                    };
                    if should_update {
                        result
                            .resolved
                            .insert(*cond_var, Shape::Bool(Some(*cond_value)));
                        result.unresolved.remove(cond_var);

                        // Also add to forced_true for forward propagation
                        forced_true.push(Constraint::Rel {
                            t1: Shape::TVar(*cond_var),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(*cond_value)),
                        });
                    }
                }
            }
        }

        // Check if we made progress
        if result.resolved.len() == prev_resolved_count {
            break;
        }
    }
}

/// Extract boolean equality constraints from a constraint (handles And wrapping)
fn extract_equalities_from_constraint(c: &Constraint) -> Vec<(usize, bool)> {
    let mut result = vec![];
    match c {
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Equality(Equality::Equal),
            t2: Shape::Bool(Some(value)),
        } => {
            result.push((*var, *value));
        }
        Constraint::And(cs) => {
            for c in cs {
                result.extend(extract_equalities_from_constraint(c));
            }
        }
        _ => {}
    }
    result
}

/// Extract nested implications from a constraint (Constraint::Conditional within And/Or)
fn extract_nested_implications(c: &Constraint, implications: &mut Vec<(Constraint, Constraint)>) {
    match c {
        Constraint::Conditional { c1, c2 } => {
            // This is an implication: c1 ==> c2
            implications.push((*c1.clone(), *c2.clone()));
            // Also extract from nested parts
            extract_nested_implications(c2, implications);
        }
        Constraint::And(cs) => {
            for inner in cs {
                extract_nested_implications(inner, implications);
            }
        }
        Constraint::Or(cs) => {
            for inner in cs {
                extract_nested_implications(inner, implications);
            }
        }
        _ => {}
    }
}

/// Propagate possibilities through subtype constraints
/// If T15 <: T2 and T15 has possibilities, propagate to T2
fn propagate_possibilities_through_subtypes(
    var_possibilities: &mut HashMap<usize, Vec<Shape>>,
    implications: &[(Constraint, Constraint)],
    resolved: &HashMap<usize, Shape>,
    substitutions: &HashMap<usize, Shape>,
) {
    let max_iterations = 10;
    for _ in 0..max_iterations {
        let mut changed = false;

        // Collect subtype constraints from implications
        for (condition, consequence) in implications {
            // Check if condition is satisfied
            let condition_satisfied = match condition {
                Constraint::Rel {
                    t1: Shape::TVar(var),
                    rel: Relation::Equality(Equality::Equal),
                    t2: value,
                } => {
                    let canonical = get_canonical_var(*var, substitutions);
                    resolved
                        .get(&canonical)
                        .map(|v| v == value)
                        .unwrap_or(false)
                }
                _ => false,
            };

            if condition_satisfied {
                // Look for subtype constraints in the consequence
                propagate_subtype_possibilities(
                    consequence,
                    var_possibilities,
                    substitutions,
                    &mut changed,
                );
            }
        }

        if !changed {
            break;
        }
    }
}

fn propagate_subtype_possibilities(
    c: &Constraint,
    var_possibilities: &mut HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
    changed: &mut bool,
) {
    match c {
        Constraint::And(cs) => {
            for inner in cs {
                propagate_subtype_possibilities(inner, var_possibilities, substitutions, changed);
            }
        }
        Constraint::Rel {
            t1: Shape::TVar(source),
            rel: Relation::Subtyping(Subtyping::Subtype),
            t2: Shape::TVar(target),
        } => {
            let canonical_source = get_canonical_var(*source, substitutions);
            let canonical_target = get_canonical_var(*target, substitutions);

            // If source has possibilities, propagate to target
            if let Some(source_types) = var_possibilities.get(&canonical_source).cloned() {
                let target_types = var_possibilities.entry(canonical_target).or_default();
                for t in source_types {
                    if !target_types.contains(&t) {
                        target_types.push(t);
                        *changed = true;
                    }
                }
            }
        }
        _ => {}
    }
}

/// Extract types from a constraint as possibilities for variables
fn extract_types_as_possibilities(
    c: &Constraint,
    var_possibilities: &mut HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
) {
    extract_types_as_possibilities_with_resolved(
        c,
        var_possibilities,
        substitutions,
        &HashMap::new(),
    );
}

/// Extract types with access to resolved variables to filter Or branches
fn extract_types_as_possibilities_with_resolved(
    c: &Constraint,
    var_possibilities: &mut HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
    resolved: &HashMap<usize, Shape>,
) {
    match c {
        Constraint::And(cs) => {
            // Collect this conjunction's own concrete equalities first, so
            // nested disjunctions are judged against branch-local facts
            // (e.g. `T21 == 1 & (T21 <: number | T21 <: string)` rules the
            // string overload out).
            let mut local_facts = resolved.clone();
            for inner in cs.iter() {
                if let Constraint::Rel {
                    t1,
                    rel: Relation::Equality(Equality::Equal),
                    t2,
                } = inner
                {
                    if let (Shape::TVar(v), t) | (t, Shape::TVar(v)) = (t1, t2) {
                        if !matches!(t, Shape::TVar(_)) {
                            let concrete = t.replace_tvars(substitutions);
                            local_facts.insert(*v, concrete.clone());
                            local_facts.insert(get_canonical_var(*v, substitutions), concrete);
                        }
                    }
                }
            }
            for inner in cs {
                extract_types_as_possibilities_with_resolved(
                    inner,
                    var_possibilities,
                    substitutions,
                    &local_facts,
                );
            }
        }
        Constraint::Or(cs) => {
            // Every branch not contradicted by *hard facts* contributes to
            // the union of possibilities. Pruning against previously
            // collected possibilities would be unsound: possibilities are
            // alternatives, not established facts (it dropped `-`'s array
            // overload because the number overload was seen first).
            //
            // Branches are processed with the branch-local machinery so a
            // variable a branch mentions but does not pin down (e.g. the
            // field type in the object branch of a lenient `.x`) keeps an
            // unknown in its union instead of being resolved from the other
            // branches alone.
            for inner in cs {
                if !or_branch_contradicted(inner, resolved, substitutions) {
                    let mut branch_poisoned: HashSet<usize> = HashSet::new();
                    let assignments =
                        extract_types_from_branch(inner, substitutions, &mut branch_poisoned);
                    for (var, shape) in assignments {
                        push_possibility(var_possibilities, var, shape);
                    }
                    for var in branch_poisoned {
                        push_possibility(var_possibilities, var, Shape::TVar(var));
                    }
                }
            }
        }
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Equality(Equality::Equal),
            t2,
        } if !matches!(t2, Shape::TVar(_)) => {
            let canonical = get_canonical_var(*var, substitutions);
            let resolved_t = t2.replace_tvars(substitutions);
            push_possibility(var_possibilities, canonical, resolved_t);
        }
        Constraint::Rel {
            t1,
            rel: Relation::Equality(Equality::Equal),
            t2: Shape::TVar(var),
        } if !matches!(t1, Shape::TVar(_)) => {
            let canonical = get_canonical_var(*var, substitutions);
            let resolved_t = t1.replace_tvars(substitutions);
            push_possibility(var_possibilities, canonical, resolved_t);
        }
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Subtyping(Subtyping::Subtype),
            t2,
        } => {
            let canonical_source = get_canonical_var(*var, substitutions);
            match t2 {
                // X <: TVar(Y) - X flows into Y
                // If we know X's possible types, Y gets those too
                Shape::TVar(target) => {
                    let canonical_target = get_canonical_var(*target, substitutions);
                    // If source has known possibilities, propagate to target
                    if let Some(source_types) = var_possibilities.get(&canonical_source).cloned() {
                        for t in source_types {
                            push_possibility(var_possibilities, canonical_target, t);
                        }
                    } else {
                        // Unresolved branch: the target may be whatever the
                        // source turns out to be. Keep the unknown in the
                        // union instead of silently dropping the branch
                        // (dropping it under-approximates the output type).
                        push_possibility(
                            var_possibilities,
                            canonical_target,
                            Shape::TVar(canonical_source),
                        );
                    }
                }
                // X <: concrete_type - X is constrained to be a subtype of that type
                // Add the generic form of that type as a possibility for X
                Shape::Number(_) => {
                    push_possibility(var_possibilities, canonical_source, Shape::Number(None));
                }
                Shape::String(_) => {
                    push_possibility(var_possibilities, canonical_source, Shape::String(None));
                }
                Shape::Bool(_) => {
                    push_possibility(var_possibilities, canonical_source, Shape::Bool(None));
                }
                // Structured bounds (arrays, tuples, objects, unions):
                // push the bound itself. Dropping the possibility would
                // under-approximate the union this variable resolves to.
                other => {
                    let resolved = other.replace_tvars(substitutions);
                    push_possibility(var_possibilities, canonical_source, resolved);
                }
            }
        }
        _ => {}
    }
}

/// Check if an Or branch is satisfiable given known possibilities
/// A branch is satisfiable if its constraints don't conflict with known types
/// A disjunction branch is *contradicted* when it contains a relation whose
/// sides are fully known (through substitutions and resolved facts) and
/// provably incompatible — e.g. the string overload of `+` when one operand
/// is the literal `1`. Unknown or partially known sides never contradict:
/// possibilities must not be used as facts.
fn or_branch_contradicted(
    c: &Constraint,
    resolved: &HashMap<usize, Shape>,
    substitutions: &HashMap<usize, Shape>,
) -> bool {
    match c {
        Constraint::And(cs) => cs
            .iter()
            .any(|i| or_branch_contradicted(i, resolved, substitutions)),
        Constraint::Or(cs) => cs
            .iter()
            .all(|i| or_branch_contradicted(i, resolved, substitutions)),
        Constraint::False => true,
        Constraint::Rel { t1, rel, t2 } => {
            let a = t1.replace_tvars(substitutions).replace_tvars(resolved);
            let b = t2.replace_tvars(substitutions).replace_tvars(resolved);
            // Judge only fully concrete sides
            if !a.dependencies().is_empty() || !b.dependencies().is_empty() {
                return false;
            }
            match rel {
                Relation::Equality(Equality::Equal) => a.disjoint_with(&b),
                Relation::Subtyping(Subtyping::Subtype) => a.disjoint_with(&b),
                _ => false,
            }
        }
        _ => false,
    }
}

#[allow(dead_code)]
fn or_branch_is_satisfiable(
    c: &Constraint,
    var_possibilities: &HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
) -> bool {
    match c {
        Constraint::And(cs) => cs
            .iter()
            .all(|inner| or_branch_is_satisfiable(inner, var_possibilities, substitutions)),
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Subtyping(Subtyping::Subtype),
            t2,
        } => {
            let canonical = get_canonical_var(*var, substitutions);
            // Check if we have possibilities for this variable
            if let Some(possibilities) = var_possibilities.get(&canonical) {
                // Check if any possibility is compatible with the constraint
                possibilities.iter().any(|p| is_type_compatible(p, t2))
            } else {
                // No possibilities known - assume satisfiable
                true
            }
        }
        _ => true, // Other constraints assumed satisfiable
    }
}

/// Check if a type is compatible with a target type for subtyping
fn is_type_compatible(source: &Shape, target: &Shape) -> bool {
    match (source, target) {
        (Shape::Number(_), Shape::Number(_)) => true,
        (Shape::String(_), Shape::String(_)) => true,
        (Shape::Bool(_), Shape::Bool(_)) => true,
        (Shape::Null, Shape::Null) => true,
        (Shape::Array(_, _), Shape::Array(_, _)) => true,
        (Shape::Object(_), Shape::Object(_)) => true,
        // TVar compatibility - conservatively assume compatible
        (Shape::TVar(_), _) | (_, Shape::TVar(_)) => true,
        _ => false,
    }
}

/// Canonicalize a shape (e.g., Union(true, false) -> Bool(None), Intersection(Number, Number) -> Number)
fn canonicalize_shape(shape: &Shape) -> Shape {
    // Single canonicalizer: Shape::canonicalize is validated against the
    // check() denotation by the model tests in tjq_testing.
    shape.canonicalize()
}

/// Check if a constraint is or contains Constraint::False
fn consequence_is_false(c: &Constraint) -> bool {
    match c {
        Constraint::False => true,
        Constraint::And(cs) => cs.iter().any(consequence_is_false),
        Constraint::Or(cs) => cs.iter().all(consequence_is_false),
        _ => false,
    }
}

/// Check if a condition constraint is satisfied by the resolved types
/// The set of falsy JSON values in jq: null and false.
fn falsy_shape() -> Shape {
    Shape::Union(Box::new(Shape::Null), Box::new(Shape::Bool(Some(false))))
}

/// Conditions of the form `X == true` / `X == false` come from conditionals
/// and are truthiness tests, not boolean equalities: jq treats every value
/// except null and false as true. For boolean-typed X the two readings
/// coincide; for non-boolean X (e.g. `if -2 then ...`) only the truthiness
/// reading is correct.
fn condition_is_satisfied(condition: &Constraint, resolved: &HashMap<usize, Shape>) -> bool {
    match condition {
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Equality(Equality::Equal),
            t2: value,
        } => {
            if let Some(resolved_val) = resolved.get(var) {
                match value {
                    // Provably truthy: disjoint from {null, false}
                    Shape::Bool(Some(true)) => {
                        resolved_val == value || resolved_val.disjoint_with(&falsy_shape())
                    }
                    // Provably falsy: included in {null, false}
                    Shape::Bool(Some(false)) => {
                        resolved_val == value || resolved_val.included_in(&falsy_shape())
                    }
                    _ => resolved_val == value,
                }
            } else {
                false
            }
        }
        Constraint::And(cs) => cs.iter().all(|c| condition_is_satisfied(c, resolved)),
        Constraint::Or(cs) => cs.iter().any(|c| condition_is_satisfied(c, resolved)),
        _ => false,
    }
}

/// Check if two constraints match (for pattern matching)
fn constraints_match(c1: &Constraint, c2: &Constraint) -> bool {
    match (c1, c2) {
        (
            Constraint::Rel {
                t1: t1a,
                rel: rel_a,
                t2: t2a,
            },
            Constraint::Rel {
                t1: t1b,
                rel: rel_b,
                t2: t2b,
            },
        ) => t1a == t1b && rel_a == rel_b && t2a == t2b,
        _ => false,
    }
}

/// Extract type assignments from a constraint and add them to the resolved types
fn extract_and_resolve_constraints(
    c: &Constraint,
    result: &mut SolverResult,
    substitutions: &HashMap<usize, Shape>,
) {
    match c {
        Constraint::And(cs) => {
            for c in cs {
                extract_and_resolve_constraints(c, result, substitutions);
            }
        }
        Constraint::Or(cs) => {
            // For disjunctions, collect all possible types
            let mut var_types: HashMap<usize, Vec<Shape>> = HashMap::new();
            let mut poisoned: HashSet<usize> = HashSet::new();
            for c in cs {
                let types = extract_types_from_branch(c, substitutions, &mut poisoned);
                for (var, shape) in types {
                    var_types.entry(var).or_default().push(shape);
                }
            }
            // Create unions for variables with multiple possibilities
            for (var, types) in var_types {
                if poisoned.contains(&var) {
                    continue; // Some branch leaves this variable unknown
                }
                if let std::collections::hash_map::Entry::Vacant(e) = result.resolved.entry(var) {
                    let mut unique_types: Vec<Shape> = vec![];
                    for t in types {
                        if !unique_types.iter().any(|existing| existing == &t) {
                            unique_types.push(t);
                        }
                    }
                    if unique_types.len() == 1 {
                        e.insert(unique_types.into_iter().next().unwrap());
                    } else if unique_types.len() > 1 {
                        let union_type = unique_types
                            .into_iter()
                            .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                            .unwrap();
                        e.insert(union_type);
                    }
                    result.unresolved.remove(&var);
                }
            }
        }
        Constraint::Rel { t1, rel, t2 } => {
            match rel {
                Relation::Equality(Equality::Equal) => {
                    match (t1, t2) {
                        (Shape::TVar(var), t) | (t, Shape::TVar(var))
                            if !matches!(t, Shape::TVar(_)) =>
                        {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            let resolved_t = t.replace_tvars(substitutions);
                            // Allow insertion or refinement of generic types to specific types
                            let should_insert = match result.resolved.get(&canonical_var) {
                                None => true,
                                // Refine Bool(None) to Bool(Some(_))
                                Some(Shape::Bool(None))
                                    if matches!(resolved_t, Shape::Bool(Some(_))) =>
                                {
                                    true
                                }
                                // Refine Number(None) to Number(Some(_))
                                Some(Shape::Number(None))
                                    if matches!(resolved_t, Shape::Number(Some(_))) =>
                                {
                                    true
                                }
                                // Refine String(None) to String(Some(_))
                                Some(Shape::String(None))
                                    if matches!(resolved_t, Shape::String(Some(_))) =>
                                {
                                    true
                                }
                                _ => false,
                            };
                            if should_insert {
                                result.resolved.insert(canonical_var, resolved_t);
                                result.unresolved.remove(&canonical_var);
                            }
                        }
                        (Shape::TVar(v1), Shape::TVar(v2)) => {
                            // If one is resolved, propagate to the other
                            let c1 = get_canonical_var(*v1, substitutions);
                            let c2 = get_canonical_var(*v2, substitutions);
                            if let Some(t) = result.resolved.get(&c1).cloned() {
                                if let std::collections::hash_map::Entry::Vacant(e) =
                                    result.resolved.entry(c2)
                                {
                                    e.insert(t);
                                    result.unresolved.remove(&c2);
                                }
                            } else if let Some(t) = result.resolved.get(&c2).cloned() {
                                if let std::collections::hash_map::Entry::Vacant(e) =
                                    result.resolved.entry(c1)
                                {
                                    e.insert(t);
                                    result.unresolved.remove(&c1);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Relation::Subtyping(Subtyping::Subtype) => {
                    // t1 <: t2 - if t1 has a known type and t2 is TVar, set t2 to t1's type
                    if let (t1_resolved, Shape::TVar(var)) = (
                        t1.replace_tvars(substitutions)
                            .replace_tvars(&result.resolved),
                        t2,
                    ) {
                        if !matches!(t1_resolved, Shape::TVar(_)) {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            if let std::collections::hash_map::Entry::Vacant(e) =
                                result.resolved.entry(canonical_var)
                            {
                                e.insert(t1_resolved);
                                result.unresolved.remove(&canonical_var);
                            }
                        }
                    }
                }
                Relation::Subtyping(Subtyping::Supertype) => {
                    // t1 :> t2 - if t2 has a known type and t1 is TVar, set t1 to t2's type
                    if let (Shape::TVar(var), t2_resolved) = (
                        t1,
                        t2.replace_tvars(substitutions)
                            .replace_tvars(&result.resolved),
                    ) {
                        if !matches!(t2_resolved, Shape::TVar(_)) {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            if let std::collections::hash_map::Entry::Vacant(e) =
                                result.resolved.entry(canonical_var)
                            {
                                e.insert(t2_resolved);
                                result.unresolved.remove(&canonical_var);
                            }
                        }
                    }
                }
                Relation::Equality(Equality::NotEqual) => {
                    // t1 != t2 - if one is TVar and other is concrete, set TVar to Neg(concrete)
                    // First resolve TVars to get actual types
                    let t1_resolved = t1
                        .replace_tvars(substitutions)
                        .replace_tvars(&result.resolved);
                    let t2_resolved = t2
                        .replace_tvars(substitutions)
                        .replace_tvars(&result.resolved);

                    match (&t1_resolved, &t2_resolved) {
                        (Shape::TVar(var), t) | (t, Shape::TVar(var))
                            if !matches!(t, Shape::TVar(_)) =>
                        {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            if let std::collections::hash_map::Entry::Vacant(e) =
                                result.resolved.entry(canonical_var)
                            {
                                // T != value means T = Neg(value)
                                e.insert(Shape::Neg(Box::new(t.clone())));
                                result.unresolved.remove(&canonical_var);
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// Extract comparison bounds from a constraint
/// Lower bounds: T >= bound (T is greater than or equal to bound)
/// Upper bounds: T < bound (T is strictly less than bound)
fn extract_comparison_bounds(
    c: &Constraint,
    lower_bounds: &mut HashMap<usize, Vec<Shape>>,
    upper_bounds: &mut HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
    resolved: &HashMap<usize, Shape>,
) {
    match c {
        Constraint::And(cs) => {
            for c in cs {
                extract_comparison_bounds(c, lower_bounds, upper_bounds, substitutions, resolved);
            }
        }
        Constraint::Or(cs) => {
            extract_comparison_bounds_from_or(
                cs,
                lower_bounds,
                upper_bounds,
                substitutions,
                resolved,
            );
        }
        Constraint::Rel { t1, rel, t2 } => {
            let t1_resolved = t1.replace_tvars(substitutions).replace_tvars(resolved);
            let t2_resolved = t2.replace_tvars(substitutions).replace_tvars(resolved);

            match rel {
                Relation::Comparison(Comparison::GreaterThan) => {
                    // t1 > t2
                    // If t1 is TVar and t2 is concrete: t1 > t2 means lower_bound for t1 is strictly greater than t2
                    // We'll track this as t1 >= t2 (lower bound) for simplicity,
                    // but mark it as strict if needed
                    if let Shape::TVar(var) = &t1_resolved {
                        if !matches!(t2_resolved, Shape::TVar(_)) {
                            let canonical = get_canonical_var(*var, substitutions);
                            let bounds = lower_bounds.entry(canonical).or_default();
                            if !bounds.contains(&t2_resolved) {
                                bounds.push(t2_resolved.clone());
                            }
                        }
                    }
                    // If t2 is TVar and t1 is concrete: t2 < t1 means upper_bound for t2 is t1
                    if let Shape::TVar(var) = &t2_resolved {
                        if !matches!(t1_resolved, Shape::TVar(_)) {
                            let canonical = get_canonical_var(*var, substitutions);
                            let bounds = upper_bounds.entry(canonical).or_default();
                            if !bounds.contains(&t1_resolved) {
                                bounds.push(t1_resolved.clone());
                            }
                        }
                    }
                }
                Relation::Comparison(Comparison::LessThan) => {
                    // t1 < t2
                    // If t1 is TVar and t2 is concrete: t1 < t2 means upper_bound for t1 is t2
                    if let Shape::TVar(var) = &t1_resolved {
                        if !matches!(t2_resolved, Shape::TVar(_)) {
                            let canonical = get_canonical_var(*var, substitutions);
                            let bounds = upper_bounds.entry(canonical).or_default();
                            if !bounds.contains(&t2_resolved) {
                                bounds.push(t2_resolved.clone());
                            }
                        }
                    }
                    // If t2 is TVar and t1 is concrete: t2 > t1 means lower_bound for t2 is t1
                    if let Shape::TVar(var) = &t2_resolved {
                        if !matches!(t1_resolved, Shape::TVar(_)) {
                            let canonical = get_canonical_var(*var, substitutions);
                            let bounds = lower_bounds.entry(canonical).or_default();
                            if !bounds.contains(&t1_resolved) {
                                bounds.push(t1_resolved.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// Extract comparison bounds from an Or constraint
/// For `t1 > t2 | t1 == t2` (i.e., t1 >= t2), extract as a lower bound
fn extract_comparison_bounds_from_or(
    options: &[Constraint],
    lower_bounds: &mut HashMap<usize, Vec<Shape>>,
    upper_bounds: &mut HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
    resolved: &HashMap<usize, Shape>,
) {
    // Check if this is a >= pattern: (t1 > t2 | t1 == t2)
    if options.len() == 2 {
        let (gt_opt, eq_opt) = match (&options[0], &options[1]) {
            (
                Constraint::Rel {
                    t1: t1a,
                    rel: Relation::Comparison(Comparison::GreaterThan),
                    t2: t2a,
                },
                Constraint::Rel {
                    t1: t1b,
                    rel: Relation::Equality(Equality::Equal),
                    t2: t2b,
                },
            ) if t1a == t1b && t2a == t2b => (Some((t1a, t2a)), Some((t1b, t2b))),
            (
                Constraint::Rel {
                    t1: t1a,
                    rel: Relation::Equality(Equality::Equal),
                    t2: t2a,
                },
                Constraint::Rel {
                    t1: t1b,
                    rel: Relation::Comparison(Comparison::GreaterThan),
                    t2: t2b,
                },
            ) if t1a == t1b && t2a == t2b => (Some((t1b, t2b)), Some((t1a, t2a))),
            _ => (None, None),
        };

        if let (Some((t1, t2)), Some(_)) = (gt_opt, eq_opt) {
            let t1_resolved = t1.replace_tvars(substitutions).replace_tvars(resolved);
            let t2_resolved = t2.replace_tvars(substitutions).replace_tvars(resolved);

            // t1 >= t2: if t1 is TVar and t2 is concrete, t2 is a lower bound for t1
            if let Shape::TVar(var) = &t1_resolved {
                if !matches!(t2_resolved, Shape::TVar(_)) {
                    let canonical = get_canonical_var(*var, substitutions);
                    let bounds = lower_bounds.entry(canonical).or_default();
                    if !bounds.contains(&t2_resolved) {
                        bounds.push(t2_resolved.clone());
                    }
                }
            }
            // t1 >= t2: if t2 is TVar and t1 is concrete, t1 is an upper bound for t2 (t2 <= t1)
            // But since it's >=, t2 could equal t1, so we don't add strict upper bound
        }
    }

    // Also process each option individually for other comparison patterns
    for opt in options {
        extract_comparison_bounds(opt, lower_bounds, upper_bounds, substitutions, resolved);
    }
}

/// Resolve type variables from their comparison bounds
/// Uses the type ordering: null < false < true < number < string < array < object
fn resolve_from_comparison_bounds(
    result: &mut SolverResult,
    lower_bounds: &HashMap<usize, Vec<Shape>>,
    upper_bounds: &HashMap<usize, Vec<Shape>>,
    substitutions: &HashMap<usize, Shape>,
) {
    for var in lower_bounds
        .keys()
        .chain(upper_bounds.keys())
        .collect::<HashSet<_>>()
    {
        if result.resolved.contains_key(var) {
            continue;
        }

        let lower = lower_bounds.get(var).cloned().unwrap_or_default();
        let upper = upper_bounds.get(var).cloned().unwrap_or_default();

        if lower.is_empty() && upper.is_empty() {
            continue;
        }

        // Find the tightest bounds
        // Lower bound: variable >= max(all lower bounds) in type ordering
        // Upper bound: variable < min(all upper bounds) in type ordering
        let narrowed = narrow_from_bounds(&lower, &upper);

        if let Some(shape) = narrowed {
            result.resolved.insert(*var, shape);
            result.unresolved.remove(var);
        }
    }
}

/// Given lower bounds (>= these) and upper bounds (< these), compute the narrowed type
fn narrow_from_bounds(lower: &[Shape], upper: &[Shape]) -> Option<Shape> {
    // Type ordering for narrowing: null < bool < number < string < array < object
    // We represent the type hierarchy as levels
    fn type_level(shape: &Shape) -> Option<u8> {
        match shape {
            Shape::Null => Some(0),
            Shape::Bool(_) => Some(1),
            Shape::Number(_) => Some(2),
            Shape::String(_) => Some(3),
            Shape::Array(_, _) | Shape::Tuple(_) => Some(4),
            Shape::Object(_) => Some(5),
            _ => None, // TVar, Blob, etc. don't have a level
        }
    }

    // Find the maximum lower bound level
    let max_lower_level = lower.iter().filter_map(|s| type_level(s)).max();

    // Find the minimum upper bound level
    let min_upper_level = upper.iter().filter_map(|s| type_level(s)).min();

    // The variable must be >= max_lower and < min_upper
    match (max_lower_level, min_upper_level) {
        (Some(low), Some(high)) if low < high => {
            // Valid range: [low, high)
            // If the range contains exactly one type level, we can narrow to that type
            if low + 1 == high {
                // Exactly one type satisfies the bounds
                level_to_shape(low)
            } else {
                // Multiple types possible - could return union, but for now return the lower bound type
                level_to_shape(low)
            }
        }
        (Some(low), Some(high)) if low >= high => {
            // Empty or invalid range - constraints may be inconsistent
            // But if low == high, the variable must be exactly at that level
            // (this happens when we have T >= X and T < Y where X and Y are at the same level)
            None
        }
        (Some(low), None) => {
            // Only lower bound - variable is at least this type
            // If the lower bound is Array and there's no upper bound,
            // the variable could be Array or Object
            // For now, return the lower bound type as the most specific
            level_to_shape(low)
        }
        (None, Some(_high)) => {
            // Only upper bound - variable is less than this type
            // Could be any type from null up to (not including) upper bound
            None // Too many possibilities
        }
        (None, None) => None,
        (Some(_), Some(_)) => None, // Catch-all for any remaining cases
    }
}

/// Convert a type level back to a Shape
fn level_to_shape(level: u8) -> Option<Shape> {
    match level {
        0 => Some(Shape::Null),
        1 => Some(Shape::Bool(None)),
        2 => Some(Shape::Number(None)),
        3 => Some(Shape::String(None)),
        4 => Some(Shape::Array(Box::new(Shape::TVar(0)), None)), // Generic array
        5 => Some(Shape::object(vec![])),
        _ => None,
    }
}

/// Check if a disjunction branch is potentially satisfiable
fn branch_is_satisfiable(
    branch: &Constraint,
    resolved: &HashMap<usize, Shape>,
    substitutions: &HashMap<usize, Shape>,
) -> bool {
    match branch {
        Constraint::And(constraints) => constraints
            .iter()
            .all(|c| branch_is_satisfiable(c, resolved, substitutions)),
        Constraint::Or(constraints) => constraints
            .iter()
            .any(|c| branch_is_satisfiable(c, resolved, substitutions)),
        Constraint::False => false,
        Constraint::Rel { t1, rel, t2 } => {
            // Substitute known values
            let t1_resolved = t1.replace_tvars(substitutions).replace_tvars(resolved);
            let t2_resolved = t2.replace_tvars(substitutions).replace_tvars(resolved);

            // An empty type (Mismatch, e.g. from contradictory bounds) has no
            // inhabitants: no input can witness this branch, even though
            // subtype relations hold vacuously for the empty set.
            if matches!(t1_resolved.canonicalize(), Shape::Mismatch(_, _))
                || matches!(t2_resolved.canonicalize(), Shape::Mismatch(_, _))
            {
                return false;
            }

            // If either type is still a TVar, the constraint is potentially satisfiable
            if matches!(t1_resolved, Shape::TVar(_)) || matches!(t2_resolved, Shape::TVar(_)) {
                return true;
            }

            // Both are concrete - check if the relation can hold for SOME
            // runtime value. A union-typed operand (e.g. `0, "b"`) satisfies
            // an overload branch when any of its members does, so only
            // provable disjointness rules a subtyping branch out.
            match rel {
                Relation::Equality(Equality::Equal) => !t1_resolved.disjoint_with(&t2_resolved),
                Relation::Equality(Equality::NotEqual) => t1_resolved != t2_resolved,
                Relation::Subtyping(Subtyping::Subtype)
                | Relation::Subtyping(Subtyping::Supertype) => {
                    !t1_resolved.disjoint_with(&t2_resolved)
                }
                Relation::Subtyping(Subtyping::Incompatible) => {
                    t1_resolved.subtype(&t2_resolved) == Subtyping::Incompatible
                }
                Relation::Comparison(_) => true, // Assume comparisons are satisfiable
            }
        }
        Constraint::Conditional { c1, c2 } => {
            // If condition is false, implication is trivially true
            // If condition is true, consequence must be satisfiable
            !branch_is_satisfiable(c1, resolved, substitutions)
                || branch_is_satisfiable(c2, resolved, substitutions)
        }
    }
}

/// Get the canonical variable for a type (following substitution chain)
fn get_canonical_var(var: usize, substitutions: &HashMap<usize, Shape>) -> usize {
    match substitutions.get(&var) {
        Some(Shape::TVar(target)) => get_canonical_var(*target, substitutions),
        _ => var,
    }
}

/// Extract type assignments from a constraint branch
fn extract_types_from_branch(
    branch: &Constraint,
    substitutions: &HashMap<usize, Shape>,
    poisoned: &mut HashSet<usize>,
) -> Vec<(usize, Shape)> {
    // Collect the branch's relations first, then resolve with a local
    // fixpoint: an equality between two variables is resolvable when either
    // side is pinned down elsewhere *in the same branch* (e.g. `+`'s null
    // overload: `T3 == null & T2 == T4` with `T4 == T3` globally). Only
    // links that stay unknown after the fixpoint poison their variables.
    let mut rels: Vec<&Constraint> = vec![];
    collect_branch_rels(branch, &mut rels);

    let mut locals: HashMap<usize, Shape> = HashMap::new();
    let mut unknown_links: Vec<(usize, usize)> = vec![];

    for rel_c in &rels {
        let Constraint::Rel { t1, rel, t2 } = rel_c else {
            continue;
        };
        match rel {
            Relation::Equality(Equality::Equal) => match (t1, t2) {
                (Shape::TVar(var), t) | (t, Shape::TVar(var)) if !matches!(t, Shape::TVar(_)) => {
                    let canonical_var = get_canonical_var(*var, substitutions);
                    let resolved_t = t.replace_tvars(substitutions);
                    locals.insert(canonical_var, resolved_t);
                }
                (Shape::TVar(v1), Shape::TVar(v2)) => {
                    let t1_resolved = t1.replace_tvars(substitutions);
                    let t2_resolved = t2.replace_tvars(substitutions);

                    if !matches!(t1_resolved, Shape::TVar(_)) {
                        locals.insert(get_canonical_var(*v2, substitutions), t1_resolved);
                    } else if !matches!(t2_resolved, Shape::TVar(_)) {
                        locals.insert(get_canonical_var(*v1, substitutions), t2_resolved);
                    } else {
                        unknown_links.push((
                            get_canonical_var(*v1, substitutions),
                            get_canonical_var(*v2, substitutions),
                        ));
                    }
                }
                _ => {}
            },
            Relation::Subtyping(Subtyping::Subtype | Subtyping::Supertype) => {
                if let Shape::TVar(var) = t1 {
                    let t2_resolved = t2.replace_tvars(substitutions);
                    if !matches!(t2_resolved, Shape::TVar(_)) {
                        let canonical_var = get_canonical_var(*var, substitutions);
                        locals.entry(canonical_var).or_insert(t2_resolved);
                    }
                }
            }
            _ => {}
        }
    }

    // Local fixpoint over the variable-variable links
    let mut changed = true;
    while changed {
        changed = false;
        for (v1, v2) in &unknown_links {
            match (locals.contains_key(v1), locals.contains_key(v2)) {
                (true, false) => {
                    let t = locals[v1].clone();
                    locals.insert(*v2, t);
                    changed = true;
                }
                (false, true) => {
                    let t = locals[v2].clone();
                    locals.insert(*v1, t);
                    changed = true;
                }
                _ => {}
            }
        }
    }

    // Any variable this branch mentions (including nested inside a shape,
    // e.g. the output variable in `input :> {a: T_out}`) but does not pin
    // down is tied to an unknown value here. Resolving it from the other
    // branches alone would drop this branch from the union, so poison it.
    for rel_c in &rels {
        let Constraint::Rel { t1, t2, .. } = rel_c else {
            continue;
        };
        for side in [t1, t2] {
            for var in side.dependencies() {
                let canonical = get_canonical_var(var, substitutions);
                if !locals.contains_key(&canonical) {
                    poisoned.insert(canonical);
                }
            }
        }
    }

    locals.into_iter().collect()
}

fn collect_branch_rels<'a>(c: &'a Constraint, out: &mut Vec<&'a Constraint>) {
    match c {
        Constraint::And(cs) => {
            for inner in cs {
                collect_branch_rels(inner, out);
            }
        }
        rel @ Constraint::Rel { .. } => out.push(rel),
        _ => {}
    }
}

/// Try to resolve a type from its subtyping bounds
fn resolve_from_bounds(fact: &Facts, substitutions: &HashMap<usize, Shape>) -> Option<Shape> {
    // Collect concrete upper bounds (supertypes - this var must be a subtype of these)
    let upper_bounds: Vec<&Shape> = fact
        .supertypes
        .iter()
        .filter(|t| !matches!(t, Shape::TVar(_)))
        .collect();

    // Collect concrete lower bounds (subtypes - these must be subtypes of this var)
    let lower_bounds: Vec<&Shape> = fact
        .subtypes
        .iter()
        .filter(|t| !matches!(t, Shape::TVar(_)))
        .collect();

    // If we have a single concrete equality, use that
    let concrete_eq: Option<&Shape> = fact
        .equalities
        .iter()
        .find(|t| !matches!(t, Shape::TVar(_)));
    if let Some(eq) = concrete_eq {
        return Some(eq.clone());
    }

    // If we have lower bounds, compute their join (least upper bound)
    if !lower_bounds.is_empty() {
        // For now, just take the first one if they're all the same kind
        // A proper implementation would compute the actual LUB
        let first = lower_bounds[0];

        // Verify all lower bounds are compatible
        let all_compatible = lower_bounds.iter().all(|b| {
            let rel = b.subtype(first);
            matches!(rel, Subtyping::Subtype | Subtyping::Supertype)
                || std::mem::discriminant(*b) == std::mem::discriminant(first)
        });

        if all_compatible {
            // Find the most general type that covers all lower bounds
            let mut result = first.clone();
            for bound in &lower_bounds[1..] {
                result = compute_lub(&result, bound);
            }
            return Some(result);
        }

        // Bounds of incompatible kinds (e.g. `T <: string` and `T <: number`
        // in the same overload branch) cannot be satisfied by a single type.
        // Surface the contradiction as a canonicalized intersection, which
        // collapses to Mismatch for disjoint types, so callers can detect the
        // unsatisfiable branch.
        let meet = lower_bounds
            .iter()
            .map(|b| (*b).clone())
            .reduce(|a, b| Shape::Intersection(Box::new(a), Box::new(b)).canonicalize())
            .unwrap();
        return Some(meet);
    }

    // If we have upper bounds, use the most specific one (GLB)
    if !upper_bounds.is_empty() {
        let first = upper_bounds[0];

        // For now, just return the first upper bound
        // A proper implementation would compute the actual GLB
        return Some(first.clone());
    }

    None
}

/// Compute the least upper bound of two types
fn compute_lub(a: &Shape, b: &Shape) -> Shape {
    match (a, b) {
        // Same type
        (a, b) if a == b => a.clone(),

        // One is subtype of the other
        (a, b) if a.subtype(b) == Subtyping::Subtype => b.clone(),
        (a, b) if a.subtype(b) == Subtyping::Supertype => a.clone(),

        // Same kind but different values - generalize
        (Shape::Bool(Some(_)), Shape::Bool(Some(_))) => Shape::Bool(None),
        (Shape::Bool(_), Shape::Bool(_)) => Shape::Bool(None),

        (Shape::Number(Some(_)), Shape::Number(Some(_))) => Shape::Number(None),
        (Shape::Number(_), Shape::Number(_)) => Shape::Number(None),

        (Shape::String(Some(_)), Shape::String(Some(_))) => Shape::String(None),
        (Shape::String(_), Shape::String(_)) => Shape::String(None),

        // Arrays - LUB of element types
        (Shape::Array(e1, len1), Shape::Array(e2, len2)) => {
            let elem_lub = compute_lub(e1, e2);
            let len = match (len1, len2) {
                (Some(l1), Some(l2)) if l1 == l2 => Some(*l1),
                _ => None,
            };
            Shape::Array(Box::new(elem_lub), len)
        }

        // Tuples - must be same length, LUB each element
        (Shape::Tuple(t1), Shape::Tuple(t2)) if t1.len() == t2.len() => {
            let elems: Vec<Shape> = t1
                .iter()
                .zip(t2.iter())
                .map(|(a, b)| compute_lub(a, b))
                .collect();
            Shape::Tuple(elems)
        }

        // Objects - intersection of fields with LUB of common field types
        (Shape::Object(o1), Shape::Object(o2)) => {
            // The join keeps the common keys (join of their value types); a
            // key present in only one side may be absent from the join's
            // inhabitants, so it becomes optional. Openness joins to open.
            let mut fields = vec![];
            for f1 in o1.iter() {
                match o2.get(&f1.key) {
                    Some(f2) => fields.push(Field {
                        key: f1.key.clone(),
                        value: compute_lub(&f1.value, &f2.value),
                        optional: f1.optional || f2.optional,
                    }),
                    None => fields.push(Field {
                        key: f1.key.clone(),
                        value: f1.value.clone(),
                        optional: true,
                    }),
                }
            }
            for f2 in o2.iter() {
                if o1.get(&f2.key).is_none() {
                    fields.push(Field {
                        key: f2.key.clone(),
                        value: f2.value.clone(),
                        optional: true,
                    });
                }
            }
            Shape::Object(Row {
                fields,
                open: o1.open || o2.open,
            })
        }

        // Incompatible types - form a union
        (a, b) => Shape::Union(Box::new(a.clone()), Box::new(b.clone())),
    }
}

// impl Display for Constraints {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         for c in &self.constraints {
//             write!(f, "{}\n", c)?;
//         }
//         Ok(())
//     }
// }

/// Typing options (docs/type-system-scope.md §5): the leniency rules jq's
/// dynamic semantics allow. Strict (all false) rejects programs that rely
/// on them; lenient matches what jq actually does at runtime.
#[derive(Debug, Clone, Copy, Default)]
pub struct TypeOptions {
    /// `.a` on null yields null instead of being a type error
    /// (the `null-index` rule).
    pub lenient_absence: bool,
}

pub struct Context {
    pub vars: usize,
    pub const_value: Json,
    pub options: TypeOptions,
    /// Lint findings raised during constraint generation (e.g. dead
    /// branches of constant-folded conditionals); merged into the
    /// SolverResult by `solve`.
    pub warnings: Vec<TypeWarning>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            vars: 0,
            const_value: Json::Null,
            options: TypeOptions::default(),
            warnings: vec![],
        }
    }

    pub fn fresh(&mut self) -> usize {
        self.vars += 1;
        self.vars
    }
}

pub fn compute_shape(
    f: &Filter,
    ctx: &mut Context,
    input_type: usize,
    output_type: usize,
    filters: &HashMap<String, Filter>,
) -> Constraints {
    compute_shape_internal(
        f,
        ctx,
        input_type,
        output_type,
        filters,
        &mut HashSet::new(),
        &mut HashMap::new(),
    )
}

/// dead-condition-branch lint for constant-folded conditionals: when a
/// conditional's condition is itself constant, the dead branch is decidable
/// right here (docs/type-system-scope.md §5).
fn lint_const_conditionals(f: &Filter, ctx: &mut Context, filters: &HashMap<String, Filter>) {
    if let Filter::IfThenElse(cond, _, _) = f {
        if cond.is_const_computable() {
            let outputs = Filter::filter(&ctx.const_value, cond, filters, &mut Default::default());
            let values: Vec<&Json> = outputs.iter().filter_map(|r| r.as_ref().ok()).collect();
            if !values.is_empty() {
                if values.iter().all(|v| v.boolify()) {
                    ctx.warnings.push(TypeWarning {
                        message: format!(
                            "condition `{cond}` is always truthy; the else branch is unreachable"
                        ),
                    });
                } else if values.iter().all(|v| !v.boolify()) {
                    ctx.warnings.push(TypeWarning {
                        message: format!(
                            "condition `{cond}` is always falsy; the then branch is unreachable"
                        ),
                    });
                }
            }
        }
    }
    // Recurse into every subterm of the constant subtree
    match f {
        Filter::Pipe(a, b) | Filter::Comma(a, b) => {
            lint_const_conditionals(a, ctx, filters);
            lint_const_conditionals(b, ctx, filters);
        }
        Filter::BinOp(l, _, r) => {
            lint_const_conditionals(l, ctx, filters);
            lint_const_conditionals(r, ctx, filters);
        }
        Filter::UnOp(_, x) => lint_const_conditionals(x, ctx, filters),
        Filter::IfThenElse(c, t, e) => {
            lint_const_conditionals(c, ctx, filters);
            lint_const_conditionals(t, ctx, filters);
            lint_const_conditionals(e, ctx, filters);
        }
        Filter::Array(items) => {
            for item in items {
                lint_const_conditionals(item, ctx, filters);
            }
        }
        Filter::Object(items) => {
            for (_, v) in items {
                lint_const_conditionals(v, ctx, filters);
            }
        }
        _ => {}
    }
}

/// Syntactic under-approximation of "this filter never fails at runtime,
/// on any input". This is the v1 of the failure effect in the arrow types
/// (docs/type-system-scope.md §2): `true` is a hard claim the differential
/// oracle checks against jq; `false` means "may fail", never the reverse.
///
/// jq facts underlying the table: comparisons and `and`/`or` are total
/// (jq's order is total across kinds); `type` and `not` are total;
/// constructors and `.`/`,`/`|` cannot themselves fail; everything
/// arithmetic, indexing, or iterating can fail on some input.
pub fn cannot_fail(f: &Filter) -> bool {
    match f {
        Filter::Dot | Filter::Null | Filter::Boolean(_) | Filter::Number(_) | Filter::String(_) => {
            true
        }
        Filter::Pipe(a, b) | Filter::Comma(a, b) => cannot_fail(a) && cannot_fail(b),
        Filter::Array(items) => items.iter().all(cannot_fail),
        Filter::Object(items) => items.iter().all(|(_, v)| cannot_fail(v)),
        Filter::IfThenElse(c, t, e) => cannot_fail(c) && cannot_fail(t) && cannot_fail(e),
        // `f?` / bare `try f` swallow every error the body raises, so they
        // never fail. `try f catch g` fails only if the handler g can.
        Filter::TryCatch(_body, None) => true,
        Filter::TryCatch(_body, Some(handler)) => cannot_fail(handler),
        Filter::BinOp(l, op, r) => {
            cannot_fail(l)
                && cannot_fail(r)
                && matches!(
                    op,
                    BinOp::Eq
                        | BinOp::Ne
                        | BinOp::Gt
                        | BinOp::Lt
                        | BinOp::Ge
                        | BinOp::Le
                        | BinOp::And
                        | BinOp::Or
                )
        }
        // `type` and `not` are total; `length` fails on booleans
        Filter::Call(name, None) => {
            // `type`, `not`, and `tostring` are total; `length` fails on
            // booleans, `keys`/`floor`/`tonumber` on wrong kinds.
            matches!(name.as_str(), "type" | "not" | "tostring")
        }
        // Negation fails on non-numbers, indexing on wrong kinds,
        // iteration on scalars; stay conservative.
        _ => false,
    }
}

/// Syntactic under-approximation of "this filter yields exactly one output
/// for every input it does not fail on". Used to decide whether an array
/// construction can be typed as a fixed-arity tuple. `false` means
/// "possibly a stream", never the reverse.
fn produces_single_output(f: &Filter) -> bool {
    match f {
        Filter::Dot | Filter::Null | Filter::Boolean(_) | Filter::Number(_) | Filter::String(_) => {
            true
        }
        // Array construction always yields exactly one array
        Filter::Array(_) => true,
        Filter::ObjIndex(inner) | Filter::ArrayIndex(inner) | Filter::UnOp(_, inner) => {
            produces_single_output(inner)
        }
        // An object with a stream-valued field yields one object per value
        Filter::Object(items) => items.iter().all(|(_, v)| produces_single_output(v)),
        Filter::BinOp(l, _, r) => produces_single_output(l) && produces_single_output(r),
        Filter::Pipe(a, b) => produces_single_output(a) && produces_single_output(b),
        Filter::IfThenElse(c, t, e) => {
            produces_single_output(c) && produces_single_output(t) && produces_single_output(e)
        }
        // Conservative allowlist of single-output builtins
        Filter::Call(name, None) => matches!(
            name.as_str(),
            "length" | "type" | "not" | "keys" | "floor" | "tostring" | "tonumber"
        ),
        _ => false,
    }
}

fn compute_shape_internal(
    f: &Filter,
    ctx: &mut Context,
    input_type: usize,
    output_type: usize,
    filters: &HashMap<String, Filter>,
    computing: &mut HashSet<String>,
    function_outputs: &mut HashMap<String, usize>,
) -> Constraints {
    if f.is_const_computable() {
        // Constant subtrees are folded whole, so their conditionals never
        // reach the solver's implication machinery; lint them here.
        lint_const_conditionals(f, ctx, filters);
        // The output type should be equal to the result of the computation
        tracing::trace!("Computing shape for constant computation: {f:?}");
        // Evaluate with the real builtin definitions: a builtin call inside
        // a constant program is still constant (`null | type`), and running
        // with empty definitions used to fail those calls, silently
        // dropping their outputs from the type.
        let output = Filter::filter(&ctx.const_value, f, filters, &mut Default::default());
        tracing::trace!("Output: {output:?}");
        // A constant program may produce several outputs (`1, 2`); the value
        // type is the union of all of them.
        let ok_shapes: Vec<Shape> = output
            .iter()
            .filter_map(|r| r.as_ref().ok())
            .map(|j| Shape::from_json(j.clone()))
            .collect();
        let any_err = output.iter().any(|r| r.is_err());
        if ok_shapes.is_empty() {
            if any_err {
                // Every evaluation fails
                return vec![Constraint::False];
            }
            // `empty`: no outputs, no constraint on the output type
            return vec![];
        }
        let output_shape = ok_shapes
            .into_iter()
            .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
            .unwrap();
        return vec![Constraint::Rel {
            t1: Shape::TVar(output_type),
            rel: Relation::Equality(Equality::Equal),
            t2: output_shape,
        }];
    }

    match f {
        Filter::Dot => {
            vec![Constraint::Rel {
                t1: Shape::TVar(input_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::TVar(output_type),
            }]
        }
        Filter::Pipe(f1, f2) => {
            let mid_type = ctx.fresh();
            let mut cs = vec![];

            cs.extend(compute_shape_internal(
                f1,
                ctx,
                input_type,
                mid_type,
                filters,
                computing,
                function_outputs,
            ));
            cs.extend(compute_shape_internal(
                f2,
                ctx,
                mid_type,
                output_type,
                filters,
                computing,
                function_outputs,
            ));

            cs
        }
        Filter::Comma(f1, f2) => {
            let left_output_type = ctx.fresh();
            let right_output_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape_internal(
                f1,
                ctx,
                input_type,
                left_output_type,
                filters,
                computing,
                function_outputs,
            ));
            cs.extend(compute_shape_internal(
                f2,
                ctx,
                input_type,
                right_output_type,
                filters,
                computing,
                function_outputs,
            ));

            // `f, g` concatenates output streams; until stream types land
            // (docs/type-system-scope.md §2) the value type is the union of
            // the two sides.
            cs.push(Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Union(
                    Box::new(Shape::TVar(left_output_type)),
                    Box::new(Shape::TVar(right_output_type)),
                ),
            });

            cs
        }
        Filter::ObjIndex(key_filter) => {
            // input_type :> { s: output_type }
            // The input must be an object with field `s` whose type is the output
            // First, try to get the field name if it's a constant string
            match key_filter.as_ref() {
                Filter::String(field_name) => {
                    // The input must be an object carrying `s`. Under lenient
                    // semantics `.a` on an object *lacking* `a` yields null
                    // rather than erroring, so the field is optional there
                    // (docs/type-system-scope.md §6); the null output is
                    // supplied by the null-input branch below. Strict mode
                    // demands the key be present.
                    let object_branch = Constraint::Rel {
                        t1: Shape::TVar(input_type),
                        rel: Relation::Subtyping(Subtyping::Supertype),
                        t2: Shape::Object(Row {
                            fields: vec![Field {
                                key: field_name.clone(),
                                value: Shape::TVar(output_type),
                                optional: ctx.options.lenient_absence,
                            }],
                            open: true,
                        }),
                    };
                    if ctx.options.lenient_absence {
                        // jq's default semantics: `.a` on null yields null
                        // (the `null-index` leniency rule,
                        // docs/type-system-scope.md §5).
                        vec![Constraint::Or(vec![
                            object_branch,
                            Constraint::And(vec![
                                Constraint::Rel {
                                    t1: Shape::TVar(input_type),
                                    rel: Relation::Equality(Equality::Equal),
                                    t2: Shape::Null,
                                },
                                Constraint::Rel {
                                    t1: Shape::TVar(output_type),
                                    rel: Relation::Equality(Equality::Equal),
                                    t2: Shape::Null,
                                },
                            ]),
                        ])]
                    } else {
                        vec![object_branch]
                    }
                }
                _ => {
                    // Dynamic field access - can't statically determine the field name
                    // Just constrain input to be an object
                    vec![]
                }
            }
        }
        Filter::ArrayIndex(n) => {
            // `.[i]`: the input is an array of some element type; the result
            // is that element, or null when the index is out of bounds. The
            // index expression is evaluated against the input and must be a
            // number.
            let elem_type = ctx.fresh();
            let index_type = ctx.fresh();
            let mut cs = compute_shape_internal(
                n,
                ctx,
                input_type,
                index_type,
                filters,
                computing,
                function_outputs,
            );
            cs.push(Constraint::Rel {
                t1: Shape::TVar(index_type),
                rel: Relation::Subtyping(Subtyping::Subtype),
                t2: Shape::Number(None),
            });

            let array_branch = Constraint::And(vec![
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Array(Box::new(Shape::TVar(elem_type)), None),
                },
                // element, or null when out of bounds
                Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Union(Box::new(Shape::TVar(elem_type)), Box::new(Shape::Null)),
                },
            ]);

            if ctx.options.lenient_absence {
                // jq's default: `.[i]` on null yields null
                cs.push(Constraint::Or(vec![
                    array_branch,
                    Constraint::And(vec![
                        Constraint::Rel {
                            t1: Shape::TVar(input_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        },
                        Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        },
                    ]),
                ]));
            } else {
                cs.push(array_branch);
            }
            cs
        }
        Filter::ArrayIterator => {
            // `.[]` iterates an array's elements or an object's values (jq
            // errors on any other input, null included). The output value
            // type is the element type for arrays; object values are
            // unconstrained here (rows carry named fields, not a uniform
            // value type), so the object branch yields the top type.
            let elem_type = ctx.fresh();
            let array_branch = Constraint::And(vec![
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Array(Box::new(Shape::TVar(elem_type)), None),
                },
                // Tie the stream's value type to the element type so element
                // constraints propagate (e.g. `.[] + 1` forces number).
                Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::TVar(elem_type),
                },
            ]);
            let object_branch = Constraint::And(vec![
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::object(vec![]),
                },
                Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Blob,
                },
            ]);
            vec![Constraint::Or(vec![array_branch, object_branch])]
        }
        Filter::Null => {
            // output_type = null
            vec![Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Null,
            }]
        }
        Filter::Boolean(b) => {
            // output_type = bool
            vec![Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Bool(Some(*b)),
            }]
        }
        Filter::Number(n) => {
            // output_type = number
            vec![Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Number(Some(*n)),
            }]
        }
        Filter::String(s) => {
            // output_type = string
            vec![Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::String(Some(s.clone())),
            }]
        }
        Filter::Array(array_filters) => {
            let (mut cs, output_types) = array_filters
                .iter()
                .map(|f| {
                    let output_type = ctx.fresh();
                    (
                        compute_shape_internal(
                            f,
                            ctx,
                            input_type,
                            output_type,
                            filters,
                            computing,
                            function_outputs,
                        ),
                        Shape::TVar(output_type),
                    )
                })
                .fold((vec![], vec![]), |(mut cs, mut output_types), (c, t)| {
                    cs.extend(c);
                    output_types.push(t);
                    (cs, output_types)
                });

            // `[f]` collects f's whole output stream. The tuple type is only
            // valid when every element filter produces exactly one value;
            // a stream-valued element (`[.[]]`, `[(1, 2)]`) collects an
            // unknown number of values, so the type widens to an array of
            // the union of the element types (empty union: unconstrained).
            let all_single = array_filters.iter().all(produces_single_output);
            let element_shape = if all_single {
                Shape::Tuple(output_types)
            } else if output_types.is_empty() {
                Shape::Array(Box::new(Shape::Blob), None)
            } else {
                let elem_union = output_types
                    .into_iter()
                    .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                    .unwrap();
                Shape::Array(Box::new(elem_union), None)
            };
            cs.push(Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: element_shape,
            });

            cs
        }
        Filter::Object(items) => {
            let (mut cs, output_types) = items
                .iter()
                .map(|(k, f)| {
                    let output_type = ctx.fresh();
                    let cs = compute_shape_internal(
                        f,
                        ctx,
                        input_type,
                        output_type,
                        filters,
                        computing,
                        function_outputs,
                    );
                    if let Filter::String(s) = k {
                        (cs, (s.clone(), Shape::TVar(output_type)))
                    } else {
                        panic!("Unsupported object key type, expected string, found {k:?}")
                    }
                })
                .fold((vec![], vec![]), |(mut cs, mut output_types), (c, t)| {
                    cs.extend(c);
                    output_types.push(t);
                    (cs, output_types)
                });

            // Object construction yields exactly these keys: a closed row.
            cs.push(Constraint::Rel {
                t1: Shape::object_closed(output_types),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::TVar(output_type),
            });

            cs
        }
        Filter::UnOp(un_op, filter) => {
            let operand_type = ctx.fresh();
            let mut cs = compute_shape_internal(
                filter,
                ctx,
                input_type,
                operand_type,
                filters,
                computing,
                function_outputs,
            );
            match un_op {
                UnOp::Neg => {
                    // The *operand's result* must be a number (the program
                    // input is only constrained through the operand itself).
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(operand_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Number(None),
                    });
                    // Negation flips the sign, which the constraint language
                    // cannot express on singletons; widen the output to
                    // number (sound, loses singleton precision).
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Number(None),
                    });
                }
            }
            cs
        }
        Filter::BinOp(lhs, bin_op, rhs) => {
            let left_type = ctx.fresh();
            let right_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape_internal(
                lhs,
                ctx,
                input_type,
                left_type,
                filters,
                computing,
                function_outputs,
            ));
            cs.extend(compute_shape_internal(
                rhs,
                ctx,
                input_type,
                right_type,
                filters,
                computing,
                function_outputs,
            ));

            match bin_op {
                BinOp::Add => {
                    // enumerate the possibilities
                    cs.push(Constraint::Or(vec![
                        // Numbers
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Number(None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Number(None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Number(None),
                            },
                        ]),
                        // Strings
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::String(None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::String(None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::String(None),
                            },
                        ]),
                        // Constraint::Rel {
                        //     t1: Shape::TVar(left_type),
                        //     rel: Relation::Equality(Equality::Equal),
                        //     t2: Shape::Null,
                        // },
                        // Constraint::Rel {
                        //     t1: Shape::TVar(right_type),
                        //     rel: Relation::Equality(Equality::Equal),
                        //     t2: Shape::Null,
                        // },
                        // Null-left
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Null,
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(right_type),
                            },
                        ]),
                        // Null-right
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Null,
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(left_type),
                            },
                        ]),
                        // Arrays concatenate
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Array(Box::new(Shape::Blob), None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Array(Box::new(Shape::Blob), None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Array(Box::new(Shape::Blob), None),
                            },
                        ]),
                        // Objects merge (right-biased)
                        Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::object(vec![]),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::object(vec![]),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::object(vec![]),
                            },
                        ]),
                    ]));

                    cs
                }
                BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    let subtype = |var: usize, t: Shape| Constraint::Rel {
                        t1: Shape::TVar(var),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: t,
                    };
                    // One overload branch: left, right, and output are each
                    // constrained to the given shape
                    let branch = |l: Shape, r: Shape, o: Shape| {
                        Constraint::And(vec![
                            subtype(left_type, l),
                            subtype(right_type, r),
                            subtype(output_type, o),
                        ])
                    };
                    let number = || Shape::Number(None);
                    let string = || Shape::String(None);
                    let array_of = |t: Shape| Shape::Array(Box::new(t), None);

                    let branches = match bin_op {
                        // Numbers subtract; arrays subtract as set difference
                        BinOp::Sub => vec![
                            branch(number(), number(), number()),
                            branch(
                                array_of(Shape::Blob),
                                array_of(Shape::Blob),
                                array_of(Shape::Blob),
                            ),
                        ],
                        // Numbers multiply; a string times a number repeats
                        // the string — but a negative count yields null
                        // (verified against jq 1.7), so the output is
                        // string | null. Two objects merge recursively.
                        BinOp::Mul => {
                            let string_or_null =
                                || Shape::Union(Box::new(string()), Box::new(Shape::Null));
                            let any_object = || Shape::object(vec![]);
                            vec![
                                branch(number(), number(), number()),
                                branch(string(), number(), string_or_null()),
                                branch(number(), string(), string_or_null()),
                                branch(any_object(), any_object(), any_object()),
                            ]
                        }
                        // Numbers divide; a string divided by a string splits it
                        BinOp::Div => vec![
                            branch(number(), number(), number()),
                            branch(string(), string(), array_of(string())),
                        ],
                        // Modulo is numbers only
                        BinOp::Mod => vec![branch(number(), number(), number())],
                        _ => unreachable!(),
                    };

                    if branches.len() == 1 {
                        // A single overload is not a disjunction; push its
                        // parts as plain constraints so they resolve directly
                        cs.extend(branches.into_iter().flat_map(|b| match b {
                            Constraint::And(inner) => inner,
                            other => vec![other],
                        }));
                    } else {
                        cs.push(Constraint::Or(branches));
                    }

                    cs
                }
                BinOp::Eq => {
                    tracing::debug!("{output_type} == true ==> {left_type} == {right_type}");
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::NotEqual),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Ne => {
                    tracing::debug!("{output_type} == true ==> {left_type} != {right_type}");
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::NotEqual),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Gt => {
                    // If the output is true, then left > right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Comparison(Comparison::GreaterThan),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    // If the output is false, then left <= right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Comparison(Comparison::LessThan),
                                t2: Shape::TVar(right_type),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(right_type),
                            },
                        ])),
                    });

                    // In any case, the output must be a bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Ge => {
                    // If the output is true, then left >= right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Comparison(Comparison::GreaterThan),
                                t2: Shape::TVar(right_type),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(right_type),
                            },
                        ])),
                    });

                    // If the output is false, then left < right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Comparison(Comparison::LessThan),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    // In any case, the output must be a bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Lt => {
                    // if the output is true, then left < right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Comparison(Comparison::LessThan),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    // if the output is false, then left >= right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Comparison(Comparison::GreaterThan),
                                t2: Shape::TVar(right_type),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(right_type),
                            },
                        ])),
                    });

                    // Output must be of type bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Le => {
                    // if the output is true, then left <= right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Comparison(Comparison::LessThan),
                                t2: Shape::TVar(right_type),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::TVar(right_type),
                            },
                        ])),
                    });

                    // if the output is false, then left > right
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Comparison(Comparison::GreaterThan),
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    // Output must be of type bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::And | BinOp::Or => {
                    // Short-circuit soundness. `or`/`and` yield a bool whenever
                    // they *succeed*, and the output type never depends on the
                    // operand types. An operand may also not be evaluated at all
                    // (`or` skips the right when the left is truthy, `and` when
                    // the left is falsy). So a const operand that always errors
                    // — e.g. `-null` in `length or (-null)` — must NOT inject a
                    // hard `False` (its whole constraint set) into the system:
                    // that `False` poisons the branch, and inside an
                    // `if/then/else` it unsoundly drops this branch's `-> bool`
                    // arrow, leaving only the other branch's codomain (the
                    // depth-7 arrow-soundness class). Dropping these `False`s can
                    // only widen the input type (inputs on which the expression
                    // in fact always errors carry no output, so tin stays sound),
                    // never let an output escape.
                    cs.retain(|c| !matches!(c, Constraint::False));
                    // jq boolifies both operands (ANY type is accepted), so the
                    // operands must NOT be constrained to bool — doing so made
                    // `. or .` wrongly demand a bool input and let the input
                    // value escape the output type.
                    //
                    // The narrowing implications below are kept: they only
                    // fire when the result is forced to a definite bool (e.g.
                    // by `... else error end`), and they tighten the operand
                    // *outputs* — for the `x >= a and x < b` guard idioms this
                    // chains through the comparison operators to narrow the
                    // input. They can only make the input type narrower (an
                    // exactness trade-off), never let an output escape.
                    let eq = |var: usize, shape: Shape| Constraint::Rel {
                        t1: Shape::TVar(var),
                        rel: Relation::Equality(Equality::Equal),
                        t2: shape,
                    };
                    // Only the `result == true` implication is emitted. The
                    // guard idiom (`then … else error`) forces the result
                    // true, and this fires forward to narrow the operands. A
                    // bare `. or .` leaves the result undetermined; with a
                    // single implication (not both polarities) the solver's
                    // possibility machinery does not explore it, so the input
                    // stays unconstrained instead of being inferred as bool.
                    // The false-polarity narrowing is dropped: it only ever
                    // tightens tin (exactness), never soundness.
                    let out_when_true: Constraint = if matches!(bin_op, BinOp::And) {
                        // and == true  => both operands truthy
                        Constraint::And(vec![
                            eq(left_type, Shape::Bool(Some(true))),
                            eq(right_type, Shape::Bool(Some(true))),
                        ])
                    } else {
                        // or == true   => at least one operand truthy
                        Constraint::Or(vec![
                            eq(left_type, Shape::Bool(Some(true))),
                            eq(right_type, Shape::Bool(Some(true))),
                        ])
                    };
                    cs.push(Constraint::Conditional {
                        c1: Box::new(eq(output_type, Shape::Bool(Some(true)))),
                        c2: Box::new(out_when_true),
                    });
                    // The result is always a bool.
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });
                    cs
                }
            }
        }
        Filter::Empty => todo!(),
        Filter::Error => {
            // output_type = error
            vec![Constraint::False]
        }
        Filter::Call(f, args) => {
            // Native builtin signatures (docs/type-system-scope.md §9): these
            // have no faithful jq-level definition. The interpreter implements
            // them natively; the axioms below keep the inference in sync.
            if args.is_none() {
                match f.as_str() {
                    // length: defined on everything but booleans (arrays and
                    // objects count, strings measure, null is 0, numbers give
                    // their absolute value); always yields a number. The
                    // defs.jq definition is an arrays-only stub.
                    "length" => {
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::Number(None),
                        }];
                    }
                    // type: total, yields one of the six type names
                    "type" => {
                        let names = ["null", "boolean", "number", "string", "array", "object"];
                        let out = names
                            .iter()
                            .map(|n| Shape::String(Some(n.to_string())))
                            .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                            .unwrap();
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: out,
                        }];
                    }
                    // not: total (negated truthiness); always a boolean. The
                    // defs.jq `if . then false else true end` lets the solver
                    // over-narrow to a singleton, so pin it here.
                    "not" => {
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(None),
                        }];
                    }
                    // keys: sorted key names for objects, indices for arrays
                    "keys" => {
                        let out = Shape::Union(
                            Box::new(Shape::Array(Box::new(Shape::String(None)), None)),
                            Box::new(Shape::Array(Box::new(Shape::Number(None)), None)),
                        );
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: out,
                        }];
                    }
                    // floor: numbers only, yields a number
                    // tonumber: numbers pass through, strings parse or fail
                    "floor" | "tonumber" => {
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Number(None),
                        }];
                    }
                    // tostring: total, always a string
                    "tostring" => {
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::String(None),
                        }];
                    }
                    // Array-consuming builtins. `sort`/`reverse`/`flatten`
                    // return an array; `add`/`min`/`max` return one of the
                    // element/aggregate types (left unconstrained). All expect
                    // an array input (jq errors otherwise).
                    "sort" | "reverse" | "flatten" => {
                        return vec![
                            Constraint::Rel {
                                t1: Shape::TVar(input_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Array(Box::new(Shape::Blob), None),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(output_type),
                                rel: Relation::Subtyping(Subtyping::Subtype),
                                t2: Shape::Array(Box::new(Shape::Blob), None),
                            },
                        ];
                    }
                    "add" | "min" | "max" => {
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(input_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::Array(Box::new(Shape::Blob), None),
                        }];
                    }
                    _ => {}
                }
            }
            if let Some(filter) = filters.get(f) {
                if let Filter::Bound(params, body) = filter {
                    // Check if we're already computing this function (recursive
                    // call). This must be gated on `computing` — the set of
                    // functions *currently on the call stack* — not merely on
                    // `function_outputs.get(f)`. Two independent, non-nested
                    // calls to the same function (e.g. `map(g) | map(h)`) both
                    // key `function_outputs` by the name `f`; without the
                    // `computing` gate the second call sees the first's lingering
                    // entry and is wrongly treated as recursive, inheriting the
                    // first call's output-type variable (a soundness bug).
                    if let Some(&recursive_output_type) =
                        function_outputs.get(f).filter(|_| computing.contains(f))
                    {
                        // This is a recursive call - create a fixpoint constraint
                        // The output type of this call equals the function's output type variable
                        tracing::trace!(
                            "Recursive call detected for function: {f}, using fixpoint output type: {}",
                            recursive_output_type
                        );
                        return vec![Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::TVar(recursive_output_type),
                        }];
                    }

                    // Add this function to the set of functions being computed
                    // and record its output type variable for fixpoint handling
                    computing.insert(f.clone());
                    function_outputs.insert(f.clone(), output_type);

                    // if params is empty, then we can compute the shape of the body directly
                    let result = if params.is_empty() {
                        compute_shape_internal(
                            body,
                            ctx,
                            input_type,
                            output_type,
                            filters,
                            computing,
                            function_outputs,
                        )
                    } else {
                        // if params is not empty, then args should match the params
                        let args = args.clone().expect("Expected args for bound filter");
                        if args.len() != params.len() {
                            panic!("Expected {} args, found {}", params.len(), args.len());
                        }

                        // Substitute each argument filter for its corresponding parameter in the body
                        let mut substituted_body = body.as_ref().clone();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            substituted_body = substituted_body.substitute(param, arg);
                        }

                        // Compute constraints on the substituted body
                        tracing::trace!("Computing shape for call: {substituted_body:?}");
                        compute_shape_internal(
                            &substituted_body,
                            ctx,
                            input_type,
                            output_type,
                            filters,
                            computing,
                            function_outputs,
                        )
                    };

                    // Pop this function off the call stack. Also drop its
                    // fixpoint output-type entry: it is only meaningful while the
                    // body is being computed (see the `computing`-gated recursion
                    // check above). Leaving it would let a later independent call
                    // to the same function reuse this call's output variable.
                    computing.remove(f);
                    function_outputs.remove(f);

                    result
                } else {
                    panic!("Expected a bound filter, found: {:?}", filter);
                }
            } else {
                // Unknown function - could be a built-in or undefined
                // For now, return empty constraints (permissive)
                // A more complete implementation would have built-in type signatures
                vec![]
            }
        }
        Filter::IfThenElse(if_, then, else_) => {
            let mut cs = vec![];

            let if_type = ctx.fresh();

            cs.extend(compute_shape_internal(
                if_,
                ctx,
                input_type,
                if_type,
                filters,
                computing,
                function_outputs,
            ));

            // jq conditions may have any type: every value except null and
            // false is truthy. The `if_type == true/false` implication
            // conditions below are interpreted as truthiness tests by the
            // solver (see condition_is_satisfied).

            let then_type = ctx.fresh();
            let then_cs = compute_shape_internal(
                then,
                ctx,
                input_type,
                then_type,
                filters,
                computing,
                function_outputs,
            );
            // if the if expression is true, then the then expression must be of type then_type
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(true)),
                }),
                c2: Box::new(Constraint::Rel {
                    t1: Shape::TVar(then_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::TVar(output_type),
                }),
            });
            // if the if expression is true, then the then expression should constrain the types.
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(true)),
                }),
                c2: Box::new(Constraint::And(then_cs.clone())),
            });

            // if the if expression is false, then the else expression must be of type else_type
            let else_type = ctx.fresh();
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(false)),
                }),
                c2: Box::new(Constraint::Rel {
                    t1: Shape::TVar(else_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::TVar(output_type),
                }),
            });
            // if the if expression is false, then the else expression should constrain the types.
            let else_cs = compute_shape_internal(
                else_,
                ctx,
                input_type,
                else_type,
                filters,
                computing,
                function_outputs,
            );
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(false)),
                }),
                c2: Box::new(Constraint::And(else_cs.clone())),
            });

            cs
        }
        Filter::Bound(items, filter) => todo!(),
        Filter::FunctionExpression(_, _) => todo!(),
        Filter::BindingExpression(lhs, _pat) => {
            // BindingExpression evaluates lhs, binds the result to variables via pattern,
            // and returns the original input (not the bound value).
            // So: output_type = input_type, but we need to ensure lhs can be evaluated.
            let mut cs = vec![];

            // Compute constraints for the left-hand side expression
            // It should be evaluable on the input_type, but we don't care about its output type
            let lhs_output_type = ctx.fresh();
            cs.extend(compute_shape_internal(
                lhs,
                ctx,
                input_type,
                lhs_output_type,
                filters,
                computing,
                function_outputs,
            ));

            // The pattern is used for runtime variable binding, but doesn't affect type inference
            // (we don't track variable types in the Context)

            // The output type equals the input type (binding expression passes through the input)
            cs.push(Constraint::Rel {
                t1: Shape::TVar(input_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::TVar(output_type),
            });

            cs
        }
        Filter::Variable(_name) => {
            // Variables are runtime-bound and we don't track variable types in the Context.
            // Without a variable type context, we can't statically determine the type.
            // Return empty constraints (permissive) - the variable could be any type.
            vec![]
        }
        // `reduce`/`foreach` fold with a bound variable and an accumulator.
        // Precisely typing the fixpoint of the update is out of scope; we type
        // them soundly as `any -> any` (an unconstrained input never claims a
        // false domain restriction, an unconstrained output never a false
        // codomain). `cannot_fail` stays conservative (both can fail).
        Filter::ReduceExpression(_, _, _, _) | Filter::ForeachExpression(_, _, _, _, _) => {
            vec![]
        }
        Filter::TryCatch(_body, _handler) => {
            // `try f [catch g]` suppresses f's failures, so f's *input*
            // constraints must not narrow the outer input (e.g. `try .a` does
            // not require an object). We type it soundly as `any -> any`:
            // leaving input and output unconstrained never claims a false
            // domain restriction nor a false codomain. Precision (unioning
            // f's and g's output shapes) is left for later; `cannot_fail`
            // already captures the failure-effect side precisely.
            vec![]
        }
        Filter::Hole => todo!(),
        Filter::SliceExpression(start, end) => {
            // Slicing an array returns an array of the same element type
            // Slicing a string returns a string
            // For simplicity: output_type = input_type (slices preserve type)
            let mut cs = vec![];

            // If start is present, it should evaluate to a number
            if let Some(start_filter) = start {
                let start_input = ctx.fresh();
                let start_output = ctx.fresh();
                cs.extend(compute_shape_internal(
                    start_filter,
                    ctx,
                    start_input,
                    start_output,
                    filters,
                    computing,
                    function_outputs,
                ));
                // start expression receives the same input
                cs.push(Constraint::Rel {
                    t1: Shape::TVar(start_input),
                    rel: Relation::Subtyping(Subtyping::Supertype),
                    t2: Shape::TVar(input_type),
                });
                // start must output a number
                cs.push(Constraint::Rel {
                    t1: Shape::TVar(start_output),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Number(None),
                });
            }

            // If end is present, it should evaluate to a number
            if let Some(end_filter) = end {
                let end_input = ctx.fresh();
                let end_output = ctx.fresh();
                cs.extend(compute_shape_internal(
                    end_filter,
                    ctx,
                    end_input,
                    end_output,
                    filters,
                    computing,
                    function_outputs,
                ));
                // end expression receives the same input
                cs.push(Constraint::Rel {
                    t1: Shape::TVar(end_input),
                    rel: Relation::Subtyping(Subtyping::Supertype),
                    t2: Shape::TVar(input_type),
                });
                // end must output a number
                cs.push(Constraint::Rel {
                    t1: Shape::TVar(end_output),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Number(None),
                });
            }

            // The sliced value is an array or a string (jq errors on other
            // kinds, null included under strict); slicing preserves the kind.
            let elem_type = ctx.fresh();
            let array_branch = Constraint::And(vec![
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Array(Box::new(Shape::TVar(elem_type)), None),
                },
                Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Array(Box::new(Shape::TVar(elem_type)), None),
                },
            ]);
            let string_branch = Constraint::And(vec![
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::String(None),
                },
                Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::String(None),
                },
            ]);
            let mut branches = vec![array_branch, string_branch];
            if ctx.options.lenient_absence {
                // jq's default: slicing null yields null
                branches.push(Constraint::And(vec![
                    Constraint::Rel {
                        t1: Shape::TVar(input_type),
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::Null,
                    },
                    Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::Null,
                    },
                ]));
            }
            cs.push(Constraint::Or(branches));

            cs
        }
    }
}

#[cfg(test)]
mod constraint_tests {
    use tjq_exec::parse;

    use super::Context;
    use crate::experimental_type_inference::{compute_shape, solve};
    use std::collections::HashMap;

    #[test]
    fn test_subtyping() {
        let (_, filter) = parse(r#".a | .b"#);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &HashMap::new());
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn test_subtyping2() {
        let (_, filter) = parse(r#"{ "a": .a, "b": .b}"#);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &HashMap::new());
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn test_subtyping3() {
        let (_, filter) = parse(r#".[3]"#);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &HashMap::new());
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn test_subtyping4() {
        let (_, filter) = parse(r#". == true or . == false"#);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &HashMap::new());
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn test_subtyping5() {
        let (_, filter) = parse(r#"if . == true or . == false then 1 else error end"#);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &HashMap::new());
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
        let _ = solve(constraints, &context);
    }
}

#[cfg(test)]
mod solver_tests {
    use std::collections::HashMap;
    use tjq_exec::{builtin_filters, parse};
    use tjq_exec::{BinOp, Filter, UnOp};

    use super::{solve, solve_arrows, Constraint, Context};

    use crate::experimental_type_inference::{compute_shape, Shape};

    fn print_constraints(constraints: &[Constraint]) {
        println!(
            "====================\n{}",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    /// Check if two shapes are equivalent (handles union order differences)
    fn shapes_equivalent(a: &Shape, b: &Shape) -> bool {
        match (a, b) {
            (Shape::Union(a1, a2), Shape::Union(b1, b2)) => {
                // Either same order or swapped
                (shapes_equivalent(a1, b1) && shapes_equivalent(a2, b2))
                    || (shapes_equivalent(a1, b2) && shapes_equivalent(a2, b1))
            }
            _ => a == b,
        }
    }

    fn solve_constraints(expression: &str) -> (Shape, Shape) {
        let _ = tracing_subscriber::fmt()
            .with_target(false)
            .with_thread_ids(false)
            .with_thread_names(false)
            .with_file(true)
            .with_line_number(true)
            .with_level(true)
            .without_time()
            .with_max_level(tracing::Level::TRACE)
            .try_init();
        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &builtin_filters());
        print_constraints(&constraints);
        let result = solve(constraints, &context).unwrap();

        // Get input and output types from the solver result
        let tin = result.get(i);
        let tout = result.get(o);

        (tin, tout)
    }

    /// Solve and return the lint warnings only.
    fn solve_warnings(expression: &str) -> Vec<String> {
        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &builtin_filters());
        let result = solve(constraints, &context).unwrap();
        result.warnings.into_iter().map(|w| w.message).collect()
    }

    #[test]
    fn test_dead_branch_lint_always_truthy() {
        // 0 is truthy in jq: the else branch can never run
        let warnings = solve_warnings(r#"if 0 then 1 else 2 end"#);
        assert!(
            warnings.iter().any(|w| w.contains("always truthy")),
            "expected always-truthy warning, got {warnings:?}"
        );
    }

    #[test]
    fn test_dead_branch_lint_always_falsy() {
        let warnings = solve_warnings(r#"if null then 1 else 2 end"#);
        assert!(
            warnings.iter().any(|w| w.contains("always falsy")),
            "expected always-falsy warning, got {warnings:?}"
        );
    }

    #[test]
    fn test_dead_branch_lint_silent_when_undetermined() {
        let warnings = solve_warnings(r#"if . == 1 then "one" else "other" end"#);
        assert!(
            warnings.is_empty(),
            "undetermined condition must not warn, got {warnings:?}"
        );
    }

    #[test]
    fn test_dead_branch_lint_spares_error_narrowing() {
        // `else error` is the narrowing idiom: the condition being forced
        // truthy is intentional, not dead code
        let warnings = solve_warnings(r#"if . == true then 1 else error end"#);
        assert!(
            warnings.is_empty(),
            "error-narrowing must not warn, got {warnings:?}"
        );
    }

    /// Solve for a filter's arrow type: an intersection of (input -> output)
    /// arrows, one per satisfiable overload branch.
    fn solve_arrow_type(expression: &str) -> Shape {
        let (_, filter) = parse(expression);
        let filter = (&filter).into();
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &builtin_filters());
        solve_arrows(constraints, &context, i, o).unwrap()
    }

    /// Flatten nested intersections into a list of component shapes
    fn intersection_members(shape: &Shape) -> Vec<&Shape> {
        match shape {
            Shape::Intersection(s1, s2) => {
                let mut members = intersection_members(s1);
                members.extend(intersection_members(s2));
                members
            }
            other => vec![other],
        }
    }

    #[test]
    fn test_solver_dot() {
        let (tin, tout) = solve_constraints(r#"."#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::TVar(1));
    }

    #[test]
    fn test_solver_number() {
        let (tin, tout) = solve_constraints(r#"3"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Number(Some(3.0)));
    }

    #[test]
    fn test_solver_boolean() {
        let (tin, tout) = solve_constraints(r#"true"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Bool(Some(true)));
    }

    #[test]
    fn test_solver_string() {
        let (tin, tout) = solve_constraints(r#""hello""#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::String(Some("hello".to_string())));
    }

    #[test]
    fn test_solver_array() {
        let (tin, tout) = solve_constraints(r#"[1, 2, 3]"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(
            tout,
            Shape::Tuple(vec![
                Shape::Number(Some(1.0)),
                Shape::Number(Some(2.0)),
                Shape::Number(Some(3.0))
            ])
        );
    }

    #[test]
    fn test_solver_object() {
        let (tin, tout) = solve_constraints(r#"{ "a": 1, "b": 2 }"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(
            tout,
            Shape::object_closed(vec![
                ("a".to_string(), Shape::Number(Some(1.0))),
                ("b".to_string(), Shape::Number(Some(2.0)))
            ])
        );
    }

    #[test]
    #[ignore]
    fn test_solver_not() {
        let (tin, tout) = solve_constraints(r#"not"#);
        // t: T -> T
        assert_eq!(
            tin,
            Shape::Union(Box::new(Shape::Null), Box::new(Shape::Bool(None)))
        );
        assert_eq!(
            tout,
            Shape::Union(Box::new(Shape::Null), Box::new(Shape::Bool(None)))
        );
    }

    #[test]
    fn test_solver_negation() {
        let (tin, tout) = solve_constraints(r#"- ."#);
        // t: T -> T
        assert_eq!(tin, Shape::Number(None));
        assert_eq!(tout, Shape::Number(None));
    }

    #[test]
    fn test_solver_add_definite() {
        let (tin, tout) = solve_constraints(r#"1 + 1"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Number(Some(2.0)));
    }

    #[test]
    fn test_solver_add_definite_nested() {
        let (tin, tout) = solve_constraints(r#"1 + 1 - 2"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Number(Some(0.0)));
    }

    #[test]
    fn test_solver_compute_string_addition() {
        let (tin, tout) = solve_constraints(r#""hello" + " world""#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::String(Some("hello world".to_string())));
    }

    #[test]
    fn test_solver_compute_array_indexing() {
        let (tin, tout) = solve_constraints(r#"[1, 2, 3] | .[0]"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Number(Some(1.0)));
    }

    #[test]
    fn test_solver_math_with_number() {
        let (tin, tout) = solve_constraints(r#". + 1"#);
        // t: (null | number) -> number
        let expected_tin = Shape::Union(Box::new(Shape::Null), Box::new(Shape::Number(None)));
        assert!(
            shapes_equivalent(&tin, &expected_tin),
            "Expected {:?}, got {:?}",
            expected_tin,
            tin
        );
        // Output should be Number(None) or a union containing Number
        assert!(
            shapes_equivalent(&tout, &Shape::Number(None)) || matches!(&tout, Shape::Union(..)),
            "Expected Number(None) or Union, got {:?}",
            tout
        );
    }

    #[test]
    fn test_solver_math_dot_dot() {
        let (tin, tout) = solve_constraints(r#". + ."#);
        // t: (null | number | string) -> (null | number | string)
        // The exact union depends on which branches are satisfiable
        let expected_tin = Shape::Union(Box::new(Shape::Null), Box::new(Shape::Number(None)));
        assert!(
            shapes_equivalent(&tin, &expected_tin) || matches!(&tin, Shape::Union(..)),
            "Expected Union type for input, got {:?}",
            tin
        );
        assert!(
            matches!(&tout, Shape::Number(_) | Shape::Union(..)),
            "Expected Number or Union for output, got {:?}",
            tout
        );
    }

    #[test]
    fn test_solver_pipe() {
        let (tin, tout) = solve_constraints(r#".a | .b"#);
        // t: { a: { b: T }} -> T
        assert_eq!(
            tin,
            Shape::object(vec![(
                "a".to_string(),
                Shape::object(vec![("b".to_string(), Shape::TVar(2))])
            )])
        );
        assert_eq!(tout, Shape::TVar(2));
    }
    #[test]
    fn test_if_bool_1_else_error() {
        let (tin, tout) = solve_constraints(r#"if . == true or . == false then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::bool_());
        assert_eq!(tout, Shape::number(1.0));
    }

    #[test]
    fn test_if_bool_1_else_error2() {
        let (tin, tout) = solve_constraints(r#"if . == true then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::bool(true));
        assert_eq!(tout, Shape::number(1.0));
    }

    #[test]
    fn test_if_bool_1_else_error3() {
        let (tin, tout) = solve_constraints(r#"if . == 5 then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::number(5));
        assert_eq!(tout, Shape::number(1.0));
    }

    #[test]
    fn test_if_bool_1_else_error4() {
        let (tin, tout) = solve_constraints(r#"if . != 5 then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::neg(Shape::number(5)));
        assert_eq!(tout, Shape::number(1));
    }

    #[test]
    fn test_length() {
        // `length` is a native builtin with an axiomatic signature
        // (docs/type-system-scope.md §9): it works on everything except
        // booleans (jq's real semantics — the defs.jq definition is an
        // arrays-only stub) and always yields a number. The input is
        // therefore unconstrained by the axiom.
        let (tin, tout) = solve_constraints(r#"length"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert!(
            matches!(tin, Shape::TVar(_)),
            "tin should be unconstrained, got: {tin}"
        );
        assert!(
            matches!(tout, Shape::Number(None)),
            "tout should be Number, got: {tout}"
        );
    }

    #[test]
    fn test_type_builtin() {
        // `type` is total and yields one of the six type names.
        let (tin, tout) = solve_constraints(r#"type"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert!(
            matches!(tin, Shape::TVar(_)),
            "tin should be unconstrained, got: {tin}"
        );
        for name in ["null", "boolean", "number", "string", "array", "object"] {
            assert!(
                Shape::String(Some(name.to_string())).included_in(&tout),
                "tout should cover \"{name}\", got: {tout}"
            );
        }
        assert!(
            tout.included_in(&Shape::String(None)),
            "tout should only contain strings, got: {tout}"
        );
    }

    #[test]
    fn test_is_array() {
        // isarray is defined as `. >= [] and . < {}` in jq
        // This narrows the input type to array because:
        // - `. >= []` is true for arrays and objects (>= array type)
        // - `. < {}` is true for null, bool, number, string, arrays (< object type)
        // The intersection is exactly: arrays
        // Since the else branch is `error`, the condition must be true, so:
        // - The input type is narrowed to array
        // - The output is exactly `1` (Number(Some(1.0)))
        let (tin, tout) = solve_constraints(r#"if . >= [] and . < {} then 1 else error end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::Array(Box::new(Shape::TVar(0)), None));
        assert_eq!(tout, Shape::number(1.0));
    }

    /// jq boolifies `and`/`or` operands, so a bare `. or .` accepts ANY
    /// input and yields a bool — the operands must not be constrained to
    /// bool. Regression for a soundness bug: constraining the operands made
    /// `tin` = bool, and in `(., (. or .))` the actual input value then
    /// escaped the inferred output type. (The guard-narrowing above is
    /// unaffected — it fires only when the result is forced by `else error`.)
    #[test]
    fn test_and_or_operands_boolified() {
        let (tin, tout) = solve_constraints(r#". or ."#);
        assert!(
            matches!(tin, Shape::TVar(_)),
            "`. or .` input must be unconstrained (any type is boolified), got {tin}"
        );
        assert!(
            matches!(tout, Shape::Bool(None)),
            "`. or .` output must be bool, got {tout}"
        );

        // The comma output must contain the input value (a type variable)
        // alongside the bool — not collapse to just bool.
        let (_, tout2) = solve_constraints(r#"(., (. or .))"#);
        let members: Vec<&Shape> = match &tout2 {
            Shape::Union(a, b) => vec![a.as_ref(), b.as_ref()],
            _ => vec![&tout2],
        };
        assert!(
            members.iter().any(|s| matches!(s, Shape::TVar(_)))
                && members.iter().any(|s| matches!(s, Shape::Bool(None))),
            "`(., (. or .))` output must be (input | bool), got {tout2}"
        );
    }

    // ==================== Intersection Type Tests ====================
    // These tests verify the set-theoretic intersection type semantics

    /// Test that overloaded operators produce intersection-of-arrow types.
    /// The `+` operator works on (Number, Number) -> Number AND (String, String) -> String,
    /// so the filter type is an intersection of arrows, keeping the correlation
    /// between input and output that the union-based solution loses.
    #[test]
    fn test_intersection_from_overloaded_operator() {
        let t = solve_arrow_type(r#". + ."#);
        tracing::debug!("filter type: {t}");
        let members = intersection_members(&t);
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number) among arrows, got {:?}",
            t
        );
        assert!(
            members.contains(&&Shape::arrow(Shape::String(None), Shape::String(None))),
            "Expected (string -> string) among arrows, got {:?}",
            t
        );
        // The null overload must not leak into the number/string arrows
        assert!(
            !members
                .iter()
                .any(|m| matches!(m, Shape::Arrow(input, _) if matches!(input.as_ref(), Shape::Union(_, _)))),
            "Arrow inputs should be branch-specific, not unions: {:?}",
            t
        );
    }

    /// `. + 1` has exactly two satisfiable overloads: the number overload and
    /// the null-input overload (null + 1 == 1). The string overload contradicts
    /// the constant `1` and must be pruned.
    #[test]
    fn test_arrow_type_add_number() {
        let t = solve_arrow_type(r#". + 1"#);
        tracing::debug!("filter type: {t}");
        let members = intersection_members(&t);
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number) among arrows, got {:?}",
            t
        );
        assert!(
            !members.iter().any(
                |m| matches!(m, Shape::Arrow(input, _) if matches!(input.as_ref(), Shape::String(_)))
            ),
            "String overload should be pruned for `. + 1`, got {:?}",
            t
        );
    }

    /// A filter with no overloading gets a single arrow, not an intersection.
    #[test]
    fn test_arrow_type_no_overload() {
        let t = solve_arrow_type(r#"- ."#);
        tracing::debug!("filter type: {t}");
        assert_eq!(t, Shape::arrow(Shape::Number(None), Shape::Number(None)));
    }

    /// Test intersection canonicalization: Number & Number = Number
    #[test]
    fn test_intersection_canonicalize_same_type() {
        use crate::shape::Shape;
        let a = Shape::Number(None);
        let b = Shape::Number(None);
        let intersection = Shape::Intersection(Box::new(a), Box::new(b));
        let canonicalized = intersection.canonicalize();
        assert_eq!(canonicalized, Shape::Number(None));
    }

    /// Test intersection canonicalization: Number(1) & Number = Number(1)
    #[test]
    fn test_intersection_canonicalize_specific_general() {
        use crate::shape::Shape;
        let a = Shape::Number(Some(1.0));
        let b = Shape::Number(None);
        let intersection = Shape::Intersection(Box::new(a), Box::new(b));
        let canonicalized = intersection.canonicalize();
        assert_eq!(canonicalized, Shape::Number(Some(1.0)));
    }

    /// Test intersection of disjoint types produces Mismatch (bottom)
    #[test]
    fn test_intersection_disjoint_types() {
        use crate::shape::Shape;
        let a = Shape::Number(None);
        let b = Shape::String(None);
        let intersection = Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()));
        let canonicalized = intersection.canonicalize();
        assert!(
            matches!(canonicalized, Shape::Mismatch(_, _)),
            "Number & String should be Mismatch (bottom), got {:?}",
            canonicalized
        );
    }

    /// Test intersection with Blob (top type): A & Blob = A
    #[test]
    fn test_intersection_with_blob() {
        use crate::shape::Shape;
        let a = Shape::Number(None);
        let b = Shape::Blob;
        let intersection = Shape::Intersection(Box::new(a.clone()), Box::new(b));
        let canonicalized = intersection.canonicalize();
        assert_eq!(canonicalized, Shape::Number(None));
    }

    /// Test Bool(true) & Bool(false) = Mismatch (empty type)
    #[test]
    fn test_intersection_contradictory_bools() {
        use crate::shape::Shape;
        let a = Shape::Bool(Some(true));
        let b = Shape::Bool(Some(false));
        let intersection = Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()));
        let canonicalized = intersection.canonicalize();
        assert!(
            matches!(canonicalized, Shape::Mismatch(_, _)),
            "Bool(true) & Bool(false) should be Mismatch, got {:?}",
            canonicalized
        );
    }

    /// Test intersection Display format
    #[test]
    fn test_intersection_display() {
        use crate::shape::Shape;
        let intersection =
            Shape::Intersection(Box::new(Shape::Number(None)), Box::new(Shape::String(None)));
        let display = format!("{}", intersection);
        assert_eq!(display, "(<number> & <string>)");
    }

    /// Test De Morgan's law: ¬(A & B) = ¬A | ¬B
    #[test]
    fn test_intersection_demorgan() {
        use crate::shape::Shape;
        let a = Shape::Number(None);
        let b = Shape::String(None);
        let intersection = Shape::Intersection(Box::new(a.clone()), Box::new(b.clone()));
        let negated = Shape::Neg(Box::new(intersection));
        let canonicalized = negated.canonicalize();
        // Should become Union(Neg(Number), Neg(String))
        assert!(
            matches!(canonicalized, Shape::Union(_, _)),
            "¬(A & B) should become Union, got {:?}",
            canonicalized
        );
    }

    /// Test if-then-else produces intersection type for conditional branches
    /// `if cond then A else B` with different input constraints produces intersection
    #[test]
    fn test_conditional_intersection() {
        // If the condition constrains input differently for then/else branches,
        // we should get an intersection type representing the conditional behavior
        let (tin, tout) = solve_constraints(r#"if . == 1 then "one" else "other" end"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        // Both branches return strings; the aggregate answer is the union of
        // the two singletons (promotion to <string> is the widening policy's
        // job, not the default — docs/type-system-scope.md §4).
        let expected = Shape::Union(
            Box::new(Shape::String(Some("one".to_string()))),
            Box::new(Shape::String(Some("other".to_string()))),
        );
        assert!(
            tout.included_in(&expected) && expected.included_in(&tout),
            "Expected \"one\" | \"other\" for conditional output, got {:?}",
            tout
        );
    }

    // ==================== Arithmetic Operator Tests ====================

    /// `. - 1` only keeps the number overload; the array overload contradicts
    /// the constant `1`
    #[test]
    fn test_arrow_type_sub_number() {
        let t = solve_arrow_type(r#". - 1"#);
        assert_eq!(t, Shape::arrow(Shape::Number(None), Shape::Number(None)));
    }

    /// `. - .` keeps both the number and the array (set difference) overloads
    #[test]
    fn test_arrow_type_sub_dot_dot() {
        let t = solve_arrow_type(r#". - ."#);
        let members = intersection_members(&t);
        let array_of_blob = || Shape::Array(Box::new(Shape::Blob), None);
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number) among arrows, got {:?}",
            t
        );
        assert!(
            members.contains(&&Shape::arrow(array_of_blob(), array_of_blob())),
            "Expected (array -> array) among arrows, got {:?}",
            t
        );
    }

    /// `. * 2` works for numbers and repeats strings. The string overload's
    /// output is `string | null`: a non-positive count yields null (jq 1.7).
    #[test]
    fn test_arrow_type_mul_number() {
        let t = solve_arrow_type(r#". * 2"#);
        let members = intersection_members(&t);
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number) among arrows, got {:?}",
            t
        );
        let string_arrow = members.iter().find(
            |m| matches!(m, Shape::Arrow(input, _) if matches!(input.as_ref(), Shape::String(_))),
        );
        match string_arrow {
            Some(Shape::Arrow(_, output)) => {
                let out_ok = output.included_in(&Shape::Union(
                    Box::new(Shape::String(None)),
                    Box::new(Shape::Null),
                )) && Shape::String(None).included_in(output);
                assert!(
                    out_ok,
                    "Expected string overload output to cover string within \
                     string | null, got {:?}",
                    t
                );
            }
            _ => panic!("Expected a string-input arrow, got {:?}", t),
        }
    }

    /// `. * .` keeps the number and object (recursive merge) overloads; the
    /// string overload needs a number on the other side, but both sides are
    /// the same input, so it is pruned.
    #[test]
    fn test_arrow_type_mul_dot_dot() {
        let t = solve_arrow_type(r#". * ."#);
        let members = intersection_members(&t);
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number) among arrows, got {:?}",
            t
        );
        assert!(
            members.contains(&&Shape::arrow(Shape::object(vec![]), Shape::object(vec![]))),
            "Expected (object -> object) among arrows, got {:?}",
            t
        );
        assert!(
            !members
                .iter()
                .any(|m| matches!(m, Shape::Arrow(i, _) if matches!(i.as_ref(), Shape::String(_)))),
            "String overload should be pruned for `. * .`, got {:?}",
            t
        );
    }

    /// `. + .`'s null overload resolves to the precise `null -> null` (not a
    /// free variable). Regression test for the Phase-2 substitution bug
    /// where a smaller-variable alias could overwrite a concrete equality on
    /// the same variable, leaving the whole equality class unresolved.
    #[test]
    fn test_arrow_type_add_dot_dot_null_precise() {
        let t = solve_arrow_type(r#". + ."#);
        let members = intersection_members(&t);
        assert!(
            members.contains(&&Shape::arrow(Shape::Null, Shape::Null)),
            "Expected precise (null -> null) among arrows, got {:?}",
            t
        );
        assert!(
            members.contains(&&Shape::arrow(Shape::Number(None), Shape::Number(None))),
            "Expected (number -> number), got {:?}",
            t
        );
    }

    /// `. / ","` is string splitting: the number overload contradicts `","`
    #[test]
    fn test_arrow_type_div_string() {
        let t = solve_arrow_type(r#". / ",""#);
        assert_eq!(
            t,
            Shape::arrow(
                Shape::String(None),
                Shape::Array(Box::new(Shape::String(None)), None)
            )
        );
    }

    /// Modulo is numbers only
    #[test]
    fn test_arrow_type_mod() {
        let t = solve_arrow_type(r#". % 2"#);
        assert_eq!(t, Shape::arrow(Shape::Number(None), Shape::Number(None)));
    }
}
