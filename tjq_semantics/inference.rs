use crate::Shape;
use std::collections::HashMap;
use tjq_exec::Filter;

/// Trait for type inference procedures.
/// Allows switching between different algorithms for inferring types from jq filters.
pub trait TypeInference {
    /// Infer the input and output types for a filter.
    /// Returns (input_shape, output_shapes).
    fn infer(&self, filter: &Filter, filters: &HashMap<String, Filter>) -> (Shape, Vec<Shape>);
}
