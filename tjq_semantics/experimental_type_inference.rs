#![allow(unused_variables)]
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter, UnOp};

use crate::{Shape, Subtyping};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Comparison {
    GreaterThan,
    LessThan,
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
            Shape::TVar(var) => equalities
                .get(var)
                .cloned()
                .unwrap_or_else(|| Shape::TVar(*var)),
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

#[derive(Debug)]
struct TypeEnv {
    equalities: HashMap<usize, Shape>, // Var -> Resolved
    subtypes: Vec<(Shape, Shape)>,     // T1 <: T2
    implications: Vec<(Constraint, Constraint)>,
    inequalities: Vec<(Shape, Shape)>, // T1 != T2
    facts: HashSet<Constraint>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            equalities: HashMap::new(),
            subtypes: vec![],
            implications: vec![],
            inequalities: vec![],
            facts: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct TypeError {
    message: String,
}

fn solve(constraints: Vec<Constraint>, ctx: &Context) -> Result<TypeEnv, TypeError> {
    let mut env = TypeEnv::new();

    for i in 0..=ctx.vars {
        env.equalities.insert(i, Shape::TVar(i));
    }

    let mut worklist = constraints;

    while let Some(c) = worklist.pop() {
        match c {
            Constraint::Rel { t1, rel, t2 } => match rel {
                Relation::Equality(Equality::Equal) => match (t1, t2) {
                    (Shape::TVar(t1), Shape::TVar(t2)) => {
                        println!("Equality: {} == {}", t1, t2);
                        match t1.cmp(&t2) {
                            Ordering::Less => env.equalities.insert(t2, Shape::TVar(t1)),
                            Ordering::Equal => continue,
                            Ordering::Greater => env.equalities.insert(t1, Shape::TVar(t2)),
                        };
                        println!("equalities: {:?}", env.equalities);
                    }
                    (Shape::TVar(t), t2) => {
                        env.equalities.insert(t, t2);
                    }
                    (t1, Shape::TVar(t)) => {
                        env.equalities.insert(t, t1);
                    }
                    _ => todo!(),
                },
                Relation::Subtyping(rel) => match rel {
                    Subtyping::Subtype => {
                        env.subtypes.push((t1, t2));
                    }
                    Subtyping::Supertype => {
                        env.subtypes.push((t2, t1));
                    }
                    Subtyping::Incompatible => todo!(),
                },
                Relation::Comparison(comparison) => todo!(),
                Relation::Equality(Equality::NotEqual) => {
                    env.inequalities.push((t1, t2));
                }
            },
            Constraint::Conditional { c1, c2 } => env.implications.push((*c1, *c2)),
            Constraint::Or(constraints) => {
                // it is false that all of the constraints are false
                worklist.push(Constraint::Conditional {
                    c1: Box::new(Constraint::And(
                        constraints.iter().map(|c| c.clone().not()).collect(),
                    )),
                    c2: Box::new(Constraint::False),
                });
            }
            Constraint::And(constraints) => {
                for c in constraints {
                    worklist.push(c);
                }
            }
            Constraint::False => todo!(),
        }
    }

    println!("TypeEnv: {:?}", env.equalities);

    // mutate all constraints to replace TVars with their resolved types

    let equalities = env.equalities.clone();
    for (u, t1) in env.equalities.iter_mut() {
        *t1 = t1.replace_tvars(&equalities);
    }

    println!("TypeEnv: {:?}", env.equalities);

    Ok(env)
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
}

impl Context {
    fn new() -> Self {
        Context { vars: 0 }
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

            cs.extend(compute_shape(f1, ctx, input_type, mid_type, filters));
            cs.extend(compute_shape(f2, ctx, mid_type, output_type, filters));

            cs
        }
        Filter::Comma(f1, f2) => {
            let left_output_type = ctx.fresh();
            let right_output_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape(
                f1,
                ctx,
                input_type,
                left_output_type,
                filters,
            ));
            cs.extend(compute_shape(
                f2,
                ctx,
                input_type,
                right_output_type,
                filters,
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
        Filter::ObjIndex(s) => {
            // input_type <: { s: output_type }
            vec![Constraint::Rel {
                t1: Shape::TVar(input_type),
                rel: Relation::Subtyping(Subtyping::Subtype),
                t2: Shape::Object(vec![(s.clone(), Shape::TVar(output_type))]),
            }]
        }
        Filter::ArrayIndex(n) => {
            // input_type <: [output_type]
            if *n >= 0 {
                vec![Constraint::Rel {
                    t1: Shape::TVar(input_type),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::Tuple(
                        [
                            vec![Shape::Blob; *n as usize],
                            vec![Shape::TVar(output_type)],
                        ]
                        .concat(),
                    ),
                }]
            } else {
                vec![
                    Constraint::Rel {
                        t1: Shape::TVar(input_type),
                        rel: Relation::Subtyping(Subtyping::Subtype),
                        t2: Shape::Array(Box::new(Shape::Blob), Some(*n)),
                    },
                    Constraint::Rel {
                        t1: Shape::TVar(output_type),
                        rel: Relation::Equality(Equality::Equal),
                        t2: Shape::Null,
                    },
                ]
            }
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
                        compute_shape(f, ctx, input_type, output_type, filters),
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
                    let cs = compute_shape(f, ctx, input_type, output_type, filters);
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
            let mut cs = compute_shape(filter, ctx, input_type, output_type, filters);
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
        Filter::BinOp(filter, bin_op, filter1) => {
            let left_type = ctx.fresh();
            let right_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape(filter, ctx, input_type, left_type, filters));
            cs.extend(compute_shape(filter1, ctx, input_type, right_type, filters));

            match bin_op {
                BinOp::Add => {
                    // If both left and right are known to be concrete values of a type, we can do type level computation
                    // let sum_num_num = Constraint::Computation {
                    //     values: vec![
                    //         |s| {
                    //             if let Shape::Number(n) = s {
                    //                 Some(n)
                    //             } else {
                    //                 None
                    //             }
                    //         },
                    //         |s| if let Shape::Number(n) = s { n } else { None },
                    //     ],
                    //     output: |values: Vec<fn(Shape) -> T>| {
                    //         if values.iter().all(|v| v.is_some()) {
                    //             Shape::Number(Some(values.iter().map(|v| v.unwrap()).sum()))
                    //         } else {
                    //             Shape::Number(None)
                    //         }
                    //     },
                    // };

                    // the types cannot be subtypes of bool
                    cs.push(
                        Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::Bool(None),
                        }
                        .not(),
                    );
                    cs.push(
                        Constraint::Rel {
                            t1: Shape::TVar(right_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::Bool(None),
                        }
                        .not(),
                    );
                    // the types must be equal, or null
                    cs.push(Constraint::Or(vec![
                        Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::TVar(right_type),
                        },
                        Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        },
                        Constraint::Rel {
                            t1: Shape::TVar(right_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        },
                    ]));
                    // if one of the types is not null, then that will be the output type
                    // note: below we encode the reverse of this logic
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(left_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::TVar(right_type),
                        }),
                    });
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Rel {
                            t1: Shape::TVar(right_type),
                            rel: Relation::Equality(Equality::Equal),
                            t2: Shape::Null,
                        }),
                        c2: Box::new(Constraint::Rel {
                            t1: Shape::TVar(output_type),
                            rel: Relation::Subtyping(Subtyping::Subtype),
                            t2: Shape::TVar(left_type),
                        }),
                    });

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
                BinOp::Ne => todo!(),
                BinOp::Gt => todo!(),
                BinOp::Ge => todo!(),
                BinOp::Lt => todo!(),
                BinOp::Le => todo!(),
                BinOp::And => todo!(),
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
                    // if params is empty, then we can compute the shape of the body directly
                    if params.is_empty() {
                        return compute_shape(body, ctx, input_type, output_type, filters);
                    }
                    // if params is not empty, then args should match the params
                    let args = args.clone().expect("Expected args for bound filter");
                    if args.len() != params.len() {
                        panic!("Expected {} args, found {}", params.len(), args.len());
                    }
                    todo!()
                } else {
                    panic!("Expected a bound filter, found: {:?}", filter);
                }
            } else {
                todo!()
            }
        }
        Filter::IfThenElse(if_, then, else_) => {
            let mut cs = vec![];

            let if_type = ctx.fresh();

            cs.extend(compute_shape(if_, ctx, input_type, if_type, filters));

            // if expression must evaluate to a boolean
            cs.push(Constraint::Rel {
                t1: Shape::TVar(if_type),
                rel: Relation::Subtyping(Subtyping::Subtype),
                t2: Shape::Bool(None),
            });

            let then_type = ctx.fresh();
            let then_cs = compute_shape(then, ctx, input_type, then_type, filters);
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
            let else_cs = compute_shape(else_, ctx, input_type, else_type, filters);
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(Some(false)),
                }),
                c2: Box::new(Constraint::And(else_cs.clone())),
            });

            // if the if expression is unknown, then the then and else expressions will be unioned
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(None),
                }),
                c2: Box::new(Constraint::Rel {
                    t1: Shape::Union(
                        Box::new(Shape::TVar(then_type)),
                        Box::new(Shape::TVar(else_type)),
                    ),
                    rel: Relation::Subtyping(Subtyping::Subtype),
                    t2: Shape::TVar(output_type),
                }),
            });

            // if the if expression is unknown, then the then and else expressions should constrain the types.
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Rel {
                    t1: Shape::TVar(if_type),
                    rel: Relation::Equality(Equality::Equal),
                    t2: Shape::Bool(None),
                }),
                c2: Box::new(Constraint::Or(vec![
                    Constraint::And(then_cs),
                    Constraint::And(else_cs),
                ])),
            });

            cs
        }
        Filter::Bound(items, filter) => todo!(),
        Filter::FunctionExpression(_, _) => todo!(),
        Filter::BindingExpression(filter, filter1) => todo!(),
        Filter::Variable(_) => todo!(),
        Filter::ReduceExpression(var_name,init , generator, update) => todo!(),
        Filter::Hole => todo!(),
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
    use tjq_exec::parse;
    use tjq_exec::{BinOp, Filter, UnOp};

    use super::{solve, Constraint, Context};
    use crate::{
        experimental_type_inference::{
            compute_shape, Equality, Relation, Shape, TypeEnv, TypeError,
        },
        Subtyping,
    };

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

        // if . == null or . == false then true else false
        let not = Filter::Bound(
            vec![],
            Box::new(Filter::if_then_else(
                Filter::or(
                    Filter::eq(Filter::Dot, Filter::Null),
                    Filter::eq(Filter::Dot, Filter::Boolean(false)),
                ),
                Filter::Boolean(true),
                Filter::Boolean(false),
            )),
        );

        let mut filters = HashMap::new();
        filters.insert("map".to_string(), map);
        filters.insert("abs".to_string(), abs);
        filters.insert("isboolean".to_string(), isboolean);
        filters.insert("type".to_string(), type_);
        filters.insert("not".to_string(), not);

        filters
    }

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

    fn solve_constraints(expression: &str) -> (Shape, Shape) {
        let (_, filter) = parse(expression);
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o, &builtin_filters());
        print_constraints(&constraints);
        let type_env = solve(constraints, &context).unwrap();
        (
            type_env.equalities.get(&i).cloned().unwrap(),
            type_env.equalities.get(&o).cloned().unwrap(),
        )
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
    #[ignore = "We cannot actually do type-level computation until we can create constraints that do computation somehow"]
    fn test_solver_add_definite() {
        let (tin, tout) = solve_constraints(r#"1 + 1"#);
        // t: T -> T
        assert_eq!(tin, Shape::TVar(1));
        assert_eq!(tout, Shape::Number(Some(2.0)));
    }

    #[test]
    #[ignore = "We can't solve subtyping constraints yet"]
    fn test_solver_math() {
        let (tin, tout) = solve_constraints(r#". + 1"#);
        // t: T -> T
        assert_eq!(
            tin,
            Shape::Union(Box::new(Shape::Null), Box::new(Shape::Number(None)))
        );
        assert_eq!(tout, Shape::Number(None));
    }

    #[test]
    #[ignore]
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
}
