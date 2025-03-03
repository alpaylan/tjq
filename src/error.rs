use std::fmt::{self, Display, Formatter};

use crate::{filter::UnOp, BinOp, Json};

#[derive(Debug, Clone, PartialEq)]
pub enum JQError {
    ObjIndexForNonObject(Json, Json),
    ArrIndexForNonArray(Json, Json),
    ArrIteratorForNonIterable(Json),
    NonStringObjectKey(Json),
    BinOpTypeError(Json, BinOp, Json),
    UnOpTypeError(Json, UnOp),
    FilterNotDefined(String, usize),
    Unknown,
}

impl Display for JQError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "jq: error(at <unknown>): ")?;

        match self {
            JQError::ObjIndexForNonObject(json, json1)
            | JQError::ArrIndexForNonArray(json, json1) => {
                write!(f, "Cannot index {} with {}", json.debug(), json1.debug())
            }
            JQError::ArrIteratorForNonIterable(json) => {
                write!(f, "Cannot iterate over {}", json.debug())
            }
            JQError::NonStringObjectKey(json) => todo!(),
            JQError::BinOpTypeError(json, bin_op, json1) => {
                write!(
                    f,
                    "{} and {} cannot be {}",
                    json.debug(),
                    json1.debug(),
                    match bin_op {
                        BinOp::Add => "added",
                        BinOp::Sub => "subtracted",
                        BinOp::Mul => "multiplied",
                        BinOp::Div => "divided",
                        BinOp::Mod => "divided (remainder)",
                        BinOp::Eq
                        | BinOp::Ne
                        | BinOp::Gt
                        | BinOp::Ge
                        | BinOp::Lt
                        | BinOp::Le
                        | BinOp::And
                        | BinOp::Or => unreachable!("{} is valid for all types", bin_op),
                    }
                )
            }
            JQError::Unknown => write!(f, "Unknown error"),
            JQError::FilterNotDefined(name, args) => {
                write!(f, "{}/{} is not defined", name, args)
            }
            JQError::UnOpTypeError(json, un_op) => {
                write!(
                    f,
                    "{} cannot be {}",
                    json.debug(),
                    match un_op {
                        UnOp::Neg => "negated",
                        UnOp::Not => "inverted",
                    }
                )
            }
        }
    }
}
