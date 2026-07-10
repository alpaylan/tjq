//! Generative testing infrastructure for the tjq type system.
//!
//! Layers (see docs/type-system-scope.md §11):
//! 1. Model tests: the shape algebra (`subtype`, `canonicalize`, set
//!    operations) validated against `Shape::check` as the denotation.
//! 2. Inhabitant generation: `inhabit` produces JSON values satisfying a
//!    `Shape`; `mutate` produces near-miss non-inhabitants.
//! 3. Program generation: random `Filter` ASTs over the subset the
//!    inference supports, pretty-printed to jq source.
//! 4. The `difftest` binary drives programs + inputs through inference and
//!    the real `jq` binary and checks the oracle properties.

pub mod filtergen;
pub mod inhabit;
pub mod jsongen;
pub mod rng;
pub mod shapegen;
pub mod shrink;

use tjq_exec::Json;
use tjq_semantics::Shape;

/// The denotation of a shape: does this JSON value inhabit it?
/// `Shape::check` is the model that every algebraic law is tested against.
pub fn denotes(shape: &Shape, j: &Json) -> bool {
    shape.check(j.clone(), vec![]).is_none()
}

/// Serialize a `Json` to valid JSON text (RFC 8259), suitable for jq stdin.
/// `Json`'s `Display` prints object keys unquoted and does not escape
/// strings, so it cannot be used for interchange.
pub fn to_json_string(j: &Json) -> String {
    match j {
        Json::Null => "null".to_string(),
        Json::Boolean(b) => b.to_string(),
        Json::Number(n) => {
            if n.is_finite() {
                // Must match tjq's own formatting exactly: jq 1.7 preserves
                // number literals from its input, so any disagreement between
                // this serializer and tjq's `to_compact_string` shows up as a
                // spurious `tostring` divergence.
                tjq_exec::canonical_number(*n)
            } else {
                // Non-finite numbers are not valid JSON; the generators never
                // produce them, but stay total.
                "null".to_string()
            }
        }
        Json::String(s) => escape_json_string(s),
        Json::Array(arr) => {
            let items: Vec<String> = arr.iter().map(to_json_string).collect();
            format!("[{}]", items.join(","))
        }
        Json::Object(obj) => {
            let items: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{}:{}", escape_json_string(k), to_json_string(v)))
                .collect();
            format!("{{{}}}", items.join(","))
        }
    }
}

/// Serialize a string as a valid JSON/jq string literal: escapes quotes,
/// backslash, and control characters (`\uXXXX`), passing printable Unicode
/// through raw (both jq and tjq accept raw UTF-8 in literals). The program
/// printer relies on this so generated string literals re-parse in jq.
pub fn escape_json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Parse JSON text (e.g. a jq output line) into `Json`.
pub fn parse_json(s: &str) -> Option<Json> {
    let v: serde_json::Value = serde_json::from_str(s).ok()?;
    Some(from_serde(&v))
}

/// Semantic JSON equality for the differential oracle: object key order is
/// irrelevant (jq's `==` semantics); array order matters. Two strings that
/// are both valid numbers of equal value are treated as equal — jq formats
/// the same double differently by provenance (`1E+308` for a preserved
/// literal, `1e+308` for a computed value), so `tostring` of an extreme
/// number differs in exponent case though the value is identical. tjq
/// stores an `f64` with no provenance and cannot replicate jq's rule, so
/// this absorbs that cosmetic difference without masking value differences.
pub fn json_equal(a: &Json, b: &Json) -> bool {
    match (a, b) {
        (Json::Object(oa), Json::Object(ob)) => {
            oa.len() == ob.len()
                && oa.iter().all(|(k, va)| {
                    ob.iter()
                        .find(|(kb, _)| kb == k)
                        .is_some_and(|(_, vb)| json_equal(va, vb))
                })
        }
        (Json::Array(aa), Json::Array(ab)) => {
            aa.len() == ab.len() && aa.iter().zip(ab).all(|(x, y)| json_equal(x, y))
        }
        (Json::String(sa), Json::String(sb)) if sa != sb => json_text_eq(sa, sb),
        _ => a == b,
    }
}

/// True when two differing strings both parse as JSON of the same value —
/// e.g. `tostring` outputs `"[5e-324]"` (computed) vs `"[5E-324]"`
/// (preserved), which are the same array once parsed. Absorbs jq's
/// provenance-dependent number formatting inside serialized structures
/// without masking value differences (a dropped element still differs).
fn json_text_eq(a: &str, b: &str) -> bool {
    match (parse_json(a), parse_json(b)) {
        (Some(ja), Some(jb)) => json_equal(&ja, &jb),
        _ => false,
    }
}

fn from_serde(v: &serde_json::Value) -> Json {
    match v {
        serde_json::Value::Null => Json::Null,
        serde_json::Value::Bool(b) => Json::Boolean(*b),
        serde_json::Value::Number(n) => Json::Number(n.as_f64().unwrap_or(f64::MAX)),
        serde_json::Value::String(s) => Json::String(s.clone()),
        serde_json::Value::Array(arr) => Json::Array(arr.iter().map(from_serde).collect()),
        serde_json::Value::Object(obj) => Json::Object(
            obj.iter()
                .map(|(k, v)| (k.clone(), from_serde(v)))
                .collect(),
        ),
    }
}
