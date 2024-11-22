use std::fmt::{self, Display, Formatter};

use crate::Filter;

#[derive(Debug, Clone)]
pub enum Constraint {
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
                Constraint::build_constraints(f2, &x)
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

