mod inference;
pub use inference::TypeInference;

mod shape;
pub use shape::*;

pub mod experimental_type_inference;
pub use experimental_type_inference::ConstraintInference;

pub mod type_inference;
