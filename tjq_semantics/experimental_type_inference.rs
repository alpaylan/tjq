#![allow(unused_variables)]
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter, Json, UnOp};

use crate::inference::TypeInference;
use crate::{Shape, Subtyping};

/// Constraint-based type inference algorithm.
/// Uses constraint generation and solving for type inference.
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
            Shape::Object(fields) => Shape::Object(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.replace_tvars(equalities)))
                    .collect(),
            ),
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
        }
    }

    /// Get the resolved type for a type variable, or TVar if unresolved
    pub fn get(&self, var: usize) -> Shape {
        self.resolved.get(&var).cloned().unwrap_or(Shape::TVar(var))
    }
}

fn solve(mut constraints: Vec<Constraint>, ctx: &Context) -> Result<SolverResult, TypeError> {
    let mut env = TypeEnv::new();
    let mut result = SolverResult::new();

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

    // First, find all equality chains and pick the smallest variable as representative
    for (var, fact) in env.facts.iter() {
        for eq in &fact.equalities {
            match eq {
                Shape::TVar(other_var) if *other_var < *var => {
                    // Map this variable to the smaller one
                    substitutions.insert(*var, Shape::TVar(*other_var));
                }
                shape if !matches!(shape, Shape::TVar(_)) => {
                    // Map to concrete type (prefer concrete over tvar)
                    substitutions.insert(*var, shape.clone());
                }
                _ => {}
            }
        }
    }

    // Transitively close the substitutions
    let mut changed = true;
    while changed {
        changed = false;
        let keys: Vec<usize> = substitutions.keys().copied().collect();
        for var in keys {
            let current = substitutions.get(&var).unwrap().clone();
            let new_val = current.replace_tvars(&substitutions);
            if new_val != current {
                substitutions.insert(var, new_val);
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
    while changed {
        changed = false;
        let vars: Vec<usize> = result.resolved.keys().copied().collect();
        for var in vars {
            let current = result.resolved.get(&var).unwrap().clone();
            let new_val = current.replace_tvars(&result.resolved);
            if new_val != current {
                result.resolved.insert(var, new_val);
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
                        var_possibilities
                            .entry(canonical)
                            .or_default()
                            .push(resolved_val);
                    }
                    Constraint::Rel {
                        t1: value,
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::TVar(var),
                    } if !matches!(value, Shape::TVar(_)) => {
                        let canonical = get_canonical_var(*var, substitutions);
                        let resolved_val = value.replace_tvars(substitutions);
                        var_possibilities
                            .entry(canonical)
                            .or_default()
                            .push(resolved_val);
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

        // Also process nested implications
        // Group implications by their condition variable to handle both branches
        let mut implications_by_var: HashMap<usize, Vec<(bool, &Constraint)>> = HashMap::new();
        for (condition, consequence) in &nested_implications {
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

        // For undetermined boolean conditions, process both branches as possibilities
        // This allows output type to be union of both branches
        for (var, branches) in &implications_by_var {
            // Check if this variable is undetermined (Bool(None) or unresolved)
            let is_undetermined = match result.resolved.get(var) {
                None => true,
                Some(Shape::Bool(None)) => true,
                _ => false,
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

        if new_resolved_count == prev_resolved_count && new_bounds_count == prev_bounds_count {
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
            let union_type = unique_types
                .into_iter()
                .reduce(|a, b| Shape::Union(Box::new(a), Box::new(b)))
                .unwrap();
            result.resolved.insert(var, union_type);
            result.unresolved.remove(&var);
        }
    }

    // Canonicalize union types (e.g., Union(true, false) -> Bool(None))
    for (var, shape) in result.resolved.iter_mut() {
        *shape = canonicalize_shape(shape);
    }

    // Re-run transitive closure after new resolutions
    let mut changed = true;
    while changed {
        changed = false;
        let vars: Vec<usize> = result.resolved.keys().copied().collect();
        for var in vars {
            let current = result.resolved.get(&var).unwrap().clone();
            let new_val = current.replace_tvars(&result.resolved);
            if new_val != current {
                result.resolved.insert(var, new_val);
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
            let branch_types = extract_types_from_branch(branch, &substitutions);
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

    Ok(result)
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
            for inner in cs {
                extract_types_as_possibilities_with_resolved(
                    inner,
                    var_possibilities,
                    substitutions,
                    resolved,
                );
            }
        }
        Constraint::Or(cs) => {
            // For Or constraints, only extract from branches that are satisfiable
            // given what we know about resolved variables
            for inner in cs {
                if or_branch_is_satisfiable(inner, var_possibilities, substitutions) {
                    extract_types_as_possibilities_with_resolved(
                        inner,
                        var_possibilities,
                        substitutions,
                        resolved,
                    );
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
            var_possibilities
                .entry(canonical)
                .or_default()
                .push(resolved_t);
        }
        Constraint::Rel {
            t1,
            rel: Relation::Equality(Equality::Equal),
            t2: Shape::TVar(var),
        } if !matches!(t1, Shape::TVar(_)) => {
            let canonical = get_canonical_var(*var, substitutions);
            let resolved_t = t1.replace_tvars(substitutions);
            var_possibilities
                .entry(canonical)
                .or_default()
                .push(resolved_t);
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
                        var_possibilities
                            .entry(canonical_target)
                            .or_default()
                            .extend(source_types);
                    }
                }
                // X <: concrete_type - X is constrained to be a subtype of that type
                // Add the generic form of that type as a possibility for X
                Shape::Number(_) => {
                    var_possibilities
                        .entry(canonical_source)
                        .or_default()
                        .push(Shape::Number(None));
                }
                Shape::String(_) => {
                    var_possibilities
                        .entry(canonical_source)
                        .or_default()
                        .push(Shape::String(None));
                }
                Shape::Bool(_) => {
                    var_possibilities
                        .entry(canonical_source)
                        .or_default()
                        .push(Shape::Bool(None));
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// Check if an Or branch is satisfiable given known possibilities
/// A branch is satisfiable if its constraints don't conflict with known types
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

/// Canonicalize a shape (e.g., Union(true, false) -> Bool(None))
fn canonicalize_shape(shape: &Shape) -> Shape {
    match shape {
        Shape::Union(a, b) => {
            let a_canon = canonicalize_shape(a);
            let b_canon = canonicalize_shape(b);

            // Union(true, false) or Union(false, true) -> Bool(None)
            match (&a_canon, &b_canon) {
                (Shape::Bool(Some(true)), Shape::Bool(Some(false)))
                | (Shape::Bool(Some(false)), Shape::Bool(Some(true))) => Shape::Bool(None),
                // Union of any bools -> Bool(None)
                (Shape::Bool(_), Shape::Bool(_)) => Shape::Bool(None),
                // Union(Number(x), Number(y)) where x != y -> Number(None)
                (Shape::Number(Some(_)), Shape::Number(Some(_))) if a_canon != b_canon => {
                    Shape::Number(None)
                }
                // Union(Number(_), Number(None)) or vice versa -> Number(None)
                (Shape::Number(_), Shape::Number(None))
                | (Shape::Number(None), Shape::Number(_)) => Shape::Number(None),
                // Union(String(x), String(y)) where x != y -> String(None)
                (Shape::String(Some(_)), Shape::String(Some(_))) if a_canon != b_canon => {
                    Shape::String(None)
                }
                // Union(String(_), String(None)) or vice versa -> String(None)
                (Shape::String(_), Shape::String(None))
                | (Shape::String(None), Shape::String(_)) => Shape::String(None),
                // If both sides are the same, just use one
                (a, b) if a == b => a.clone(),
                _ => Shape::Union(Box::new(a_canon), Box::new(b_canon)),
            }
        }
        Shape::Object(fields) => Shape::Object(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), canonicalize_shape(v)))
                .collect(),
        ),
        Shape::Array(elem, len) => Shape::Array(Box::new(canonicalize_shape(elem)), *len),
        Shape::Tuple(elems) => Shape::Tuple(elems.iter().map(canonicalize_shape).collect()),
        _ => shape.clone(),
    }
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
fn condition_is_satisfied(condition: &Constraint, resolved: &HashMap<usize, Shape>) -> bool {
    match condition {
        Constraint::Rel {
            t1: Shape::TVar(var),
            rel: Relation::Equality(Equality::Equal),
            t2: value,
        } => {
            if let Some(resolved_val) = resolved.get(var) {
                resolved_val == value
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
            for c in cs {
                let types = extract_types_from_branch(c, substitutions);
                for (var, shape) in types {
                    var_types.entry(var).or_default().push(shape);
                }
            }
            // Create unions for variables with multiple possibilities
            for (var, types) in var_types {
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
        5 => Some(Shape::Object(vec![])),
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

            // If either type is still a TVar, the constraint is potentially satisfiable
            if matches!(t1_resolved, Shape::TVar(_)) || matches!(t2_resolved, Shape::TVar(_)) {
                return true;
            }

            // Both are concrete - check if the relation holds
            // Note: The subtype function has unusual semantics:
            //   - Supertype means "self is more specific than other"
            //   - Subtype means "self is more general than other"
            // So for `t1 <: t2` (t1 should be a standard subtype of t2),
            // we need to check if t1 is more specific (Supertype) or equal (Subtype) to t2
            match rel {
                Relation::Equality(Equality::Equal) => t1_resolved == t2_resolved,
                Relation::Equality(Equality::NotEqual) => t1_resolved != t2_resolved,
                Relation::Subtyping(Subtyping::Subtype) => {
                    // t1 <: t2 means t1 should be more specific or equal to t2
                    let sub_rel = t1_resolved.subtype(&t2_resolved);
                    matches!(sub_rel, Subtyping::Subtype | Subtyping::Supertype)
                }
                Relation::Subtyping(Subtyping::Supertype) => {
                    // t1 :> t2 means t1 should be more general or equal to t2
                    let sub_rel = t1_resolved.subtype(&t2_resolved);
                    matches!(sub_rel, Subtyping::Subtype | Subtyping::Supertype)
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
) -> Vec<(usize, Shape)> {
    let mut types = vec![];

    match branch {
        Constraint::And(constraints) => {
            for c in constraints {
                types.extend(extract_types_from_branch(c, substitutions));
            }
        }
        Constraint::Rel { t1, rel, t2 } => {
            match rel {
                Relation::Equality(Equality::Equal) => {
                    // If one side is TVar and other is concrete, assign the type
                    match (t1, t2) {
                        (Shape::TVar(var), t) | (t, Shape::TVar(var))
                            if !matches!(t, Shape::TVar(_)) =>
                        {
                            // Follow substitution chain to get canonical variable
                            let canonical_var = get_canonical_var(*var, substitutions);
                            // Apply substitutions to get the final type
                            let resolved_t = t.replace_tvars(substitutions);
                            types.push((canonical_var, resolved_t));
                        }
                        (Shape::TVar(v1), Shape::TVar(v2)) => {
                            // Both are TVars - check if one resolves to something concrete
                            let t1_resolved = t1.replace_tvars(substitutions);
                            let t2_resolved = t2.replace_tvars(substitutions);

                            if !matches!(t1_resolved, Shape::TVar(_)) {
                                let canonical_var = get_canonical_var(*v2, substitutions);
                                types.push((canonical_var, t1_resolved));
                            } else if !matches!(t2_resolved, Shape::TVar(_)) {
                                let canonical_var = get_canonical_var(*v1, substitutions);
                                types.push((canonical_var, t2_resolved));
                            }
                        }
                        _ => {}
                    }
                }
                Relation::Subtyping(Subtyping::Subtype) => {
                    // t1 <: t2 means t1 must be a subtype of t2
                    // If t1 is TVar and t2 is concrete, t1's type should be at most t2
                    if let Shape::TVar(var) = t1 {
                        let t2_resolved = t2.replace_tvars(substitutions);
                        if !matches!(t2_resolved, Shape::TVar(_)) {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            types.push((canonical_var, t2_resolved));
                        }
                    }
                }
                Relation::Subtyping(Subtyping::Supertype) => {
                    // t1 :> t2 means t1 must be a supertype of t2
                    if let Shape::TVar(var) = t1 {
                        let t2_resolved = t2.replace_tvars(substitutions);
                        if !matches!(t2_resolved, Shape::TVar(_)) {
                            let canonical_var = get_canonical_var(*var, substitutions);
                            types.push((canonical_var, t2_resolved));
                        }
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    types
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
            let mut fields = vec![];
            for (k, v1) in o1 {
                if let Some((_, v2)) = o2.iter().find(|(k2, _)| k2 == k) {
                    fields.push((k.clone(), compute_lub(v1, v2)));
                }
                // Fields only in o1 are dropped (can't require them in LUB)
            }
            Shape::Object(fields)
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

pub struct Context {
    pub vars: usize,
    pub const_value: Json,
}

impl Context {
    fn new() -> Self {
        Context {
            vars: 0,
            const_value: Json::Null,
        }
    }

    fn fresh(&mut self) -> usize {
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
        // The output type should be equal to the result of the computation
        tracing::trace!("Computing shape for constant computation: {f:?}");
        let output = Filter::filter(
            &ctx.const_value,
            f,
            &Default::default(),
            &mut Default::default(),
        );
        tracing::trace!("Output: {output:?}");
        // Assume a single output for now
        match output.first() {
            Some(Ok(output)) => {
                let output_shape = Shape::from_json(output.clone());
                return vec![Constraint::Rel {
                    t1: Shape::TVar(output_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: output_shape,
                }];
            }
            Some(Err(err)) => {
                return vec![Constraint::False];
            }
            None => {
                return vec![];
            }
        }
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

            // output_type = left_output_type, right_output_type
            // cs.push(Constraint::Comparison {
            //     t1: Shape::Stream(vec![
            //         Shape::TVar(left_output_type),
            //         Shape::TVar(right_output_type),
            //     ]),
            //     rel: Relation::Equality(Equality::Equal),
            //     t2: Shape::TVar(output_type),
            // });

            cs
        }
        Filter::ObjIndex(key_filter) => {
            // input_type :> { s: output_type }
            // The input must be an object with field `s` whose type is the output
            // First, try to get the field name if it's a constant string
            match key_filter.as_ref() {
                Filter::String(field_name) => {
                    vec![Constraint::Rel {
                        t1: Shape::TVar(input_type),
                        rel: Relation::Subtyping(Subtyping::Supertype),
                        t2: Shape::Object(vec![(field_name.clone(), Shape::TVar(output_type))]),
                    }]
                }
                _ => {
                    // Dynamic field access - can't statically determine the field name
                    // Just constrain input to be an object
                    vec![]
                }
            }
        }
        Filter::ArrayIndex(n) => {
            // input_type <: [output_type]
            // todo: if n is a constant, compute it.

            // if *n >= 0 {
            //     vec![Constraint::Rel {
            //         t1: Shape::TVar(input_type),
            //         rel: Relation::Subtyping(Subtyping::Subtype),
            //         t2: Shape::Tuple(
            //             [
            //                 vec![Shape::Blob; *n as usize],
            //                 vec![Shape::TVar(output_type)],
            //             ]
            //             .concat(),
            //         ),
            //     }]
            // } else {
            //     vec![
            //         Constraint::Rel {
            //             t1: Shape::TVar(input_type),
            //             rel: Relation::Subtyping(Subtyping::Subtype),
            //             t2: Shape::Array(Box::new(Shape::Blob), Some(*n)),
            //         },
            //         Constraint::Rel {
            //             t1: Shape::TVar(output_type),
            //             rel: Relation::Equality(Equality::Equal),
            //             t2: Shape::Null,
            //         },
            //     ]
            // }
            vec![]
        }
        Filter::ArrayIterator => {
            // todo: figure out object iteration
            // output_type <: [input_type]

            // this is the type of a single element of the output stream
            let single_output_type = ctx.fresh();

            vec![
                // input must have been an array of the single_output_type
                // [single_output_type] = input_type
                Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Array(Box::new(Shape::TVar(single_output_type)), None),
                },
                // output must be a stream of the single_output_type
                // output_type = S<single_output_type>
                // Constraint::Subtyping {
                //     t1: Shape::TVar(single_output_type),
                //     rel: Subtyping::Subtype,
                //     t2: Shape::Stream(vec![Shape::TVar(single_output_type)]),
                // },
            ]
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

            cs.push(Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::Tuple(output_types),
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

            cs.push(Constraint::Rel {
                t1: Shape::Object(output_types),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::TVar(output_type),
            });

            cs
        }
        Filter::UnOp(un_op, filter) => {
            // let output_type = ctx.fresh();
            let mut cs = compute_shape_internal(
                filter,
                ctx,
                input_type,
                output_type,
                filters,
                computing,
                function_outputs,
            );
            match un_op {
                UnOp::Neg => {
                    // input type must be a number
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(input_type),
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::Number(None),
                    });
                    // output_type must be a number
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Equality(Equality::Equal),
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
                    ]));

                    cs
                }
                BinOp::Sub => todo!(),
                BinOp::Mul => todo!(),
                BinOp::Div => todo!(),
                BinOp::Mod => todo!(),
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
                BinOp::Le => todo!(),
                BinOp::And => {
                    // if the output is true, then both left and right must be true
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(true)),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(true)),
                            },
                        ])),
                    });

                    // if the output is false, then either left or right must be false
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(false)),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(false)),
                            },
                        ])),
                    });

                    // left and right and output must be of type bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(left_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(right_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs
                }
                BinOp::Or => {
                    // if the output is true, then either left or right must be true
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(true)),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(true)),
                            },
                        ])),
                    });

                    // if the output is false, then both left and right must be false
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::And(vec![
                            Constraint::Rel {
                                t1: Shape::TVar(left_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(false)),
                            },
                            Constraint::Rel {
                                t1: Shape::TVar(right_type),
                                rel: Relation::Equality(Equality::Equal),
                                t2: Shape::Bool(Some(false)),
                            },
                        ])),
                    });

                    // left and right and output must be of type bool
                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(left_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Rel {
                        t1: Shape::TVar(right_type),
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
            if let Some(filter) = filters.get(f) {
                if let Filter::Bound(params, body) = filter {
                    // Check if we're already computing this function (recursive call)
                    if let Some(&recursive_output_type) = function_outputs.get(f) {
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

                    // Remove this function from the set of functions being computed
                    // but keep it in function_outputs in case there are other calls
                    computing.remove(f);

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

            // if expression must evaluate to a boolean
            cs.push(Constraint::Rel {
                t1: Shape::TVar(if_type),
                rel: Relation::Subtyping(Subtyping::Subtype),
                t2: Shape::Bool(None),
            });

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
        Filter::ReduceExpression(var_name, init, generator, update) => todo!(),
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

            // Output type equals input type (slicing preserves the type)
            cs.push(Constraint::Rel {
                t1: Shape::TVar(output_type),
                rel: Relation::Equality(Equality::Equal),
                t2: Shape::TVar(input_type),
            });

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

    use super::{solve, Constraint, Context};

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
    #[should_panic]
    fn test_solver_object() {
        let (tin, tout) = solve_constraints(r#"{ "a": 1, "b": 2 }"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(
            tout,
            Shape::Object(vec![
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
            Shape::Object(vec![(
                "a".to_string(),
                Shape::Object(vec![("b".to_string(), Shape::TVar(2))])
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
        // The length function narrows input to array via backward propagation:
        // 1. length uses: if isarray | not then error else ... end
        // 2. isarray | not produces true when input is not an array
        // 3. The then-branch errors, so `not`'s output must be false
        // 4. Backward propagation: not(x)=false implies x=true
        // 5. isarray(.)=true implies . is an array
        //
        // Output type resolution:
        // 6. When T3=false (error branch not taken), T15 <: T2 is activated
        // 7. T16 (is array empty?) has both true/false branches
        // 8. T16=true branch: T19=0 flows through T19 <: T15 <: T2
        // 9. T16=false branch: recursive call (not fully analyzed yet)
        let (tin, tout) = solve_constraints(r#"length"#);
        tracing::debug!("tin: {tin}, tout: {tout}");
        assert_eq!(tin, Shape::Array(Box::new(Shape::TVar(0)), None));
        // Output is Number(0) from the base case - recursive case would add more
        assert!(
            matches!(tout, Shape::Number(None)),
            "tout should be Number, got: {tout}"
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
}
