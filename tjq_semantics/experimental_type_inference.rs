use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

use tjq_exec::{BinOp, Filter};

use crate::{Shape, Subtyping};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Comparison::Equal => write!(f, "=="),
            Comparison::NotEqual => write!(f, "!="),
            Comparison::GreaterThan => write!(f, ">"),
            Comparison::GreaterThanOrEqual => write!(f, ">="),
            Comparison::LessThan => write!(f, "<"),
            Comparison::LessThanOrEqual => write!(f, "<="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Subtyping {
        t1: Shape,
        rel: Subtyping,
        t2: Shape,
    },
    Comparison {
        t1: Shape,
        rel: Comparison,
        t2: Shape,
    },
    Conditional {
        c1: Box<Constraint>,
        c2: Box<Constraint>,
    },
    Or(Vec<Constraint>),
    And(Vec<Constraint>),
    Error,
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Subtyping { t1, rel, t2 } => match rel {
                Subtyping::Subtype => write!(f, "{} <: {}", t1, t2),
                Subtyping::Supertype => write!(f, "{} <: {}", t2, t1),
                Subtyping::Incompatible => unreachable!(),
            },
            Constraint::Conditional { c1, c2 } => {
                write!(f, "{} ==> {}", c1, c2)
            }
            Constraint::Comparison { t1, rel, t2 } => {
                write!(f, "{} {} {}", t1, rel, t2)
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
            Constraint::Error => {
                write!(f, "error")
            }
        }
    }
}

impl Constraint {
    pub fn dependencies(&self) -> Vec<usize> {
        match self {
            Constraint::Subtyping { t1, t2, .. } => {
                let mut deps = t1.dependencies();
                deps.extend(t2.dependencies());
                deps
            }
            Constraint::Comparison { t1, t2, .. } => {
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
            Constraint::Error => vec![],
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

struct TypeError {
    message: String,
}

fn solve(constraints: Vec<Constraint>, ctx: &Context) -> Result<TypeEnv, TypeError> {
    let mut env = TypeEnv::new();

    for i in 0..ctx.vars {
        env.equalities.insert(i, Shape::TVar(i));
    }

    let mut worklist = constraints;

    while let Some(c) = worklist.pop() {
        match c {
            Constraint::Comparison {
                t1,
                rel: Comparison::Equal,
                t2,
            } => match (t1, t2) {
                (Shape::TVar(t1), Shape::TVar(t2)) => {
                    match t1.cmp(&t2) {
                        Ordering::Less => env.equalities.insert(t2, Shape::TVar(t1)),
                        Ordering::Equal => continue,
                        Ordering::Greater => env.equalities.insert(t1, Shape::TVar(t2)),
                    };
                }
                (Shape::TVar(t), t2) => {
                    env.equalities.insert(t, t2);
                }
                (t1, Shape::TVar(t)) => {
                    env.equalities.insert(t, t1);
                }
                _ => todo!(),
            },
            Constraint::Comparison { t1, rel, t2 } => todo!(),
            Constraint::Subtyping { t1, rel, t2 } => match rel {
                Subtyping::Subtype => {
                    env.subtypes.push((t1, t2));
                }
                Subtyping::Supertype => {
                    env.subtypes.push((t2, t1));
                }
                Subtyping::Incompatible => todo!(),
            },
            Constraint::Conditional { c1, c2 } => env.implications.push((*c1, *c2)),
            Constraint::Or(constraints) => todo!(),
            Constraint::And(constraints) => todo!(),
            Constraint::Error => todo!(),
        }
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
) -> Constraints {
    match f {
        Filter::Dot => {
            vec![Constraint::Comparison {
                t1: Shape::TVar(input_type),
                rel: Comparison::Equal,
                t2: Shape::TVar(output_type),
            }]
        }
        Filter::Pipe(f1, f2) => {
            let mid_type = ctx.fresh();
            let mut cs = vec![];

            cs.extend(compute_shape(f1, ctx, input_type, mid_type));
            cs.extend(compute_shape(f2, ctx, mid_type, output_type));

            cs
        }
        Filter::Comma(f1, f2) => {
            let left_output_type = ctx.fresh();
            let right_output_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape(f1, ctx, input_type, left_output_type));
            cs.extend(compute_shape(f2, ctx, input_type, right_output_type));

            // output_type = left_output_type, right_output_type
            // cs.push(Constraint::Comparison {
            //     t1: Shape::Stream(vec![
            //         Shape::TVar(left_output_type),
            //         Shape::TVar(right_output_type),
            //     ]),
            //     rel: Comparison::Equal,
            //     t2: Shape::TVar(output_type),
            // });

            cs
        }
        Filter::ObjIndex(s) => {
            // input_type <: { s: output_type }
            vec![Constraint::Subtyping {
                t1: Shape::TVar(input_type),
                rel: Subtyping::Subtype,
                t2: Shape::Object(vec![(s.clone(), Shape::TVar(output_type))]),
            }]
        }
        Filter::ArrayIndex(n) => {
            // input_type <: [output_type]
            if *n >= 0 {
                vec![Constraint::Subtyping {
                    t1: Shape::TVar(input_type),
                    rel: Subtyping::Subtype,
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
                    Constraint::Subtyping {
                        t1: Shape::TVar(input_type),
                        rel: Subtyping::Subtype,
                        t2: Shape::Array(Box::new(Shape::Blob), Some(*n)),
                    },
                    Constraint::Comparison {
                        t1: Shape::TVar(output_type),
                        rel: Comparison::Equal,
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
                Constraint::Subtyping {
                    t1: Shape::TVar(input_type),
                    rel: Subtyping::Subtype,
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
            vec![Constraint::Comparison {
                t1: Shape::TVar(output_type),
                rel: Comparison::Equal,
                t2: Shape::Null,
            }]
        }
        Filter::Boolean(b) => {
            // output_type = bool
            vec![Constraint::Comparison {
                t1: Shape::TVar(output_type),
                rel: Comparison::Equal,
                t2: Shape::Bool(Some(*b)),
            }]
        }
        Filter::Number(n) => {
            // output_type = number
            vec![Constraint::Comparison {
                t1: Shape::TVar(output_type),
                rel: Comparison::Equal,
                t2: Shape::Number(Some(*n)),
            }]
        }
        Filter::String(s) => {
            // output_type = string
            vec![Constraint::Comparison {
                t1: Shape::TVar(output_type),
                rel: Comparison::Equal,
                t2: Shape::String(Some(s.clone())),
            }]
        }
        Filter::Array(filters) => {
            let (mut cs, output_types) = filters
                .iter()
                .map(|f| {
                    let output_type = ctx.fresh();
                    (
                        compute_shape(f, ctx, input_type, output_type),
                        Shape::TVar(output_type),
                    )
                })
                .fold((vec![], vec![]), |(mut cs, mut output_types), (c, t)| {
                    cs.extend(c);
                    output_types.push(t);
                    (cs, output_types)
                });

            // cs.push(Constraint::Comparison {
            //     t1: Shape::Array(Box::new(Shape::Stream(output_types)), None),
            //     rel: Comparison::Equal,
            //     t2: Shape::TVar(output_type),
            // });

            cs
        }
        Filter::Object(items) => {
            let (mut cs, output_types) = items
                .iter()
                .map(|(k, f)| {
                    let output_type = ctx.fresh();
                    let cs = compute_shape(f, ctx, input_type, output_type);
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

            cs.push(Constraint::Comparison {
                t1: Shape::Object(output_types),
                rel: Comparison::Equal,
                t2: Shape::TVar(output_type),
            });

            cs
        }
        Filter::UnOp(un_op, filter) => todo!(),
        Filter::BinOp(filter, bin_op, filter1) => {
            let left_type = ctx.fresh();
            let right_type = ctx.fresh();

            let mut cs = vec![];
            cs.extend(compute_shape(filter, ctx, input_type, left_type));
            cs.extend(compute_shape(filter1, ctx, input_type, right_type));

            match bin_op {
                BinOp::Add => {
                    todo!()
                }
                BinOp::Sub => todo!(),
                BinOp::Mul => todo!(),
                BinOp::Div => todo!(),
                BinOp::Mod => todo!(),
                BinOp::Eq => {
                    tracing::debug!("{output_type} == true ==> {left_type} == {right_type}");
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(output_type),
                            rel: Comparison::Equal,
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(left_type),
                            rel: Comparison::Equal,
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(output_type),
                            rel: Comparison::Equal,
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(left_type),
                            rel: Comparison::NotEqual,
                            t2: Shape::TVar(right_type),
                        }),
                    });

                    cs.push(Constraint::Subtyping {
                        t1: Shape::TVar(output_type),
                        rel: Subtyping::Subtype,
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
                        c1: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(output_type),
                            rel: Comparison::Equal,
                            t2: Shape::Bool(Some(true)),
                        }),
                        c2: Box::new(Constraint::Or(vec![
                            Constraint::Comparison {
                                t1: Shape::TVar(left_type),
                                rel: Comparison::Equal,
                                t2: Shape::Bool(Some(true)),
                            },
                            Constraint::Comparison {
                                t1: Shape::TVar(right_type),
                                rel: Comparison::Equal,
                                t2: Shape::Bool(Some(true)),
                            },
                        ])),
                    });

                    // if the output is false, then both left and right must be false
                    cs.push(Constraint::Conditional {
                        c1: Box::new(Constraint::Comparison {
                            t1: Shape::TVar(output_type),
                            rel: Comparison::Equal,
                            t2: Shape::Bool(Some(false)),
                        }),
                        c2: Box::new(Constraint::And(vec![
                            Constraint::Comparison {
                                t1: Shape::TVar(left_type),
                                rel: Comparison::Equal,
                                t2: Shape::Bool(Some(false)),
                            },
                            Constraint::Comparison {
                                t1: Shape::TVar(right_type),
                                rel: Comparison::Equal,
                                t2: Shape::Bool(Some(false)),
                            },
                        ])),
                    });

                    // left and right and output must be of type bool
                    cs.push(Constraint::Subtyping {
                        t1: Shape::TVar(output_type),
                        rel: Subtyping::Subtype,
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Subtyping {
                        t1: Shape::TVar(left_type),
                        rel: Subtyping::Subtype,
                        t2: Shape::Bool(None),
                    });

                    cs.push(Constraint::Subtyping {
                        t1: Shape::TVar(right_type),
                        rel: Subtyping::Subtype,
                        t2: Shape::Bool(None),
                    });

                    cs
                }
            }
        }
        Filter::Empty => todo!(),
        Filter::Error => {
            // output_type = error
            vec![Constraint::Error]
        }
        Filter::Call(_, filters) => todo!(),
        Filter::IfThenElse(if_, then, else_) => {
            let mut cs = vec![];

            let if_type = ctx.fresh();

            cs.extend(compute_shape(if_, ctx, input_type, if_type));

            // if expression must evaluate to a boolean
            cs.push(Constraint::Subtyping {
                t1: Shape::TVar(if_type),
                rel: Subtyping::Subtype,
                t2: Shape::Bool(None),
            });

            let then_type = ctx.fresh();
            let then_cs = compute_shape(then, ctx, input_type, then_type);
            // if the if expression is true, then the then expression must be of type then_type
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
                    t2: Shape::Bool(Some(true)),
                }),
                c2: Box::new(Constraint::Subtyping {
                    t1: Shape::TVar(then_type),
                    rel: Subtyping::Subtype,
                    t2: Shape::TVar(output_type),
                }),
            });
            // if the if expression is true, then the then expression should constrain the types.
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
                    t2: Shape::Bool(Some(true)),
                }),
                c2: Box::new(Constraint::And(then_cs.clone())),
            });

            // if the if expression is false, then the else expression must be of type else_type
            let else_type = ctx.fresh();
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
                    t2: Shape::Bool(Some(false)),
                }),
                c2: Box::new(Constraint::Subtyping {
                    t1: Shape::TVar(else_type),
                    rel: Subtyping::Subtype,
                    t2: Shape::TVar(output_type),
                }),
            });
            // if the if expression is false, then the else expression should constrain the types.
            let else_cs = compute_shape(else_, ctx, input_type, else_type);
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
                    t2: Shape::Bool(Some(false)),
                }),
                c2: Box::new(Constraint::And(else_cs.clone())),
            });

            // if the if expression is unknown, then the then and else expressions will be unioned
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
                    t2: Shape::Bool(None),
                }),
                c2: Box::new(Constraint::Subtyping {
                    t1: Shape::Union(
                        Box::new(Shape::TVar(then_type)),
                        Box::new(Shape::TVar(else_type)),
                    ),
                    rel: Subtyping::Subtype,
                    t2: Shape::TVar(output_type),
                }),
            });

            // if the if expression is unknown, then the then and else expressions should constrain the types.
            cs.push(Constraint::Conditional {
                c1: Box::new(Constraint::Comparison {
                    t1: Shape::TVar(if_type),
                    rel: Comparison::Equal,
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
    }
}

#[cfg(test)]
mod constraint_tests {
    use tjq_exec::{BinOp, Filter};

    use crate::experimental_type_inference::{compute_shape, solve};

    use super::{Context, Shape};

    #[test]
    fn test_subtyping() {
        // .a | .b
        let filter = Filter::Pipe(
            Box::new(Filter::ObjIndex("a".into())),
            Box::new(Filter::ObjIndex("b".into())),
        );
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o);
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
        // let (_, filter) = parse(r#"{ "a": .a, "b": .b}"#);
        let filter = Filter::Object(vec![
            ("a".into(), Filter::ObjIndex("a".into())),
            ("b".into(), Filter::ObjIndex("b".into())),
        ]);
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o);
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
        // .[3]
        let filter = Filter::ArrayIndex(3);
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o);
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
        // let (_, filter) = parse(r#". == true or . == false"#);
        let filter = Filter::BinOp(
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
        );
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o);
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
        // let (_, filter) = parse(r#"if . == true or . == false then 1 else error end"#);
        let filter = Filter::IfThenElse(
            Box::new(Filter::BinOp(
                Box::new(Filter::Dot),
                BinOp::Eq,
                Box::new(Filter::Boolean(true)),
            )),
            Box::new(Filter::Number(1.0)),
            Box::new(Filter::Error),
        );
        let mut context = Context::new();
        let i = context.fresh();
        let o = context.fresh();
        let constraints = compute_shape(&filter, &mut context, i, o);
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
