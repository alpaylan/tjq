use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Json {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(Vec<(String, Json)>),
}

impl From<i32> for Json {
    fn from(value: i32) -> Self {
        Json::Number(value as f64)
    }
}

impl From<f64> for Json {
    fn from(value: f64) -> Self {
        Json::Number(value)
    }
}

impl From<String> for Json {
    fn from(value: String) -> Self {
        Json::String(value)
    }
}

impl From<&str> for Json {
    fn from(value: &str) -> Self {
        Json::String(value.to_string())
    }
}

impl From<bool> for Json {
    fn from(value: bool) -> Self {
        Json::Boolean(value)
    }
}

impl<T> From<Vec<T>> for Json
where
    T: Into<Json>,
{
    fn from(value: Vec<T>) -> Self {
        Json::Array(value.into_iter().map(Into::into).collect())
    }
}

impl<T> From<Vec<(&str, T)>> for Json
where
    T: Into<Json>,
{
    fn from(value: Vec<(&str, T)>) -> Self {
        Json::Object(
            value
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.into()))
                .collect(),
        )
    }
}

impl<T> From<Option<T>> for Json
where
    T: Into<Json>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => v.into(),
            None => Json::Null,
        }
    }
}

impl Json {
    pub fn boolify(&self) -> bool {
        match self {
            Json::Boolean(b) => *b,
            Json::Null => false,
            _ => true,
        }
    }

    pub fn from_serde_value(value: serde_json::Value) -> Self {
        match value {
            serde_json::Value::Null => Json::Null,
            serde_json::Value::Bool(b) => Json::Boolean(b),
            serde_json::Value::Number(n) => Json::Number(n.as_f64().unwrap()),
            serde_json::Value::String(s) => Json::String(s),
            serde_json::Value::Array(arr) => {
                Json::Array(arr.into_iter().map(Json::from_serde_value).collect())
            }
            serde_json::Value::Object(obj) => Json::Object(
                obj.into_iter()
                    .map(|(k, v)| (k, Json::from_serde_value(v)))
                    .collect(),
            ),
        }
    }
}

impl PartialOrd for Json {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Json {}
impl Ord for Json {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // JSON values are ordered as follows:
        // null
        // false
        // true
        // numbers
        // strings, in alphabetical order (by unicode codepoint value)
        // arrays, in lexical order
        // objects
        match (self, other) {
            (Json::Null, Json::Null) => std::cmp::Ordering::Equal,
            (Json::Null, _) => std::cmp::Ordering::Less,
            (_, Json::Null) => std::cmp::Ordering::Greater,
            (Json::Boolean(b1), Json::Boolean(b2)) => b1.cmp(b2),
            (Json::Boolean(_), _) => std::cmp::Ordering::Less,
            (_, Json::Boolean(_)) => std::cmp::Ordering::Greater,
            (Json::Number(n1), Json::Number(n2)) => n1.partial_cmp(n2).unwrap(),
            (Json::Number(_), _) => std::cmp::Ordering::Less,
            (_, Json::Number(_)) => std::cmp::Ordering::Greater,
            (Json::String(s1), Json::String(s2)) => s1.cmp(s2),
            (Json::String(_), _) => std::cmp::Ordering::Less,
            (_, Json::String(_)) => std::cmp::Ordering::Greater,
            (Json::Array(a1), Json::Array(a2)) => {
                for (j1, j2) in a1.iter().zip(a2.iter()) {
                    match j1.cmp(j2) {
                        std::cmp::Ordering::Equal => continue,
                        other => return other,
                    }
                }

                a1.len().cmp(&a2.len())
            }
            (Json::Array(_), _) => std::cmp::Ordering::Less,
            (_, Json::Array(_)) => std::cmp::Ordering::Greater,
            (Json::Object(o1), Json::Object(o2)) => {
                // jq compares objects by their *sorted* key arrays first,
                // and only on equal key sets compares values in sorted key
                // order (insertion order is never significant).
                let mut k1: Vec<&String> = o1.iter().map(|(k, _)| k).collect();
                let mut k2: Vec<&String> = o2.iter().map(|(k, _)| k).collect();
                k1.sort();
                k2.sort();
                match k1.cmp(&k2) {
                    std::cmp::Ordering::Equal => {
                        for k in k1 {
                            let j1 = o1
                                .iter()
                                .find(|(kk, _)| kk == k)
                                .map(|(_, v)| v)
                                .expect("key present");
                            let j2 = o2
                                .iter()
                                .find(|(kk, _)| kk == k)
                                .map(|(_, v)| v)
                                .expect("key present");
                            match j1.cmp(j2) {
                                std::cmp::Ordering::Equal => continue,
                                other => return other,
                            }
                        }
                        std::cmp::Ordering::Equal
                    }
                    other => other,
                }
            }
        }
    }
}

impl Display for Json {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Json::Null => write!(f, "null"),
            Json::Boolean(b) => write!(f, "{}", b),
            Json::Number(n) => write!(f, "{}", n),
            Json::String(s) => write!(f, "\"{}\"", s),
            Json::Array(arr) => {
                write!(f, "[")?;
                for (i, j) in arr.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", j)?;
                }
                write!(f, "]")
            }
            Json::Object(obj) => {
                write!(f, "{{")?;
                for (i, (key, value)) in obj.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

/// Canonical number text: integer-valued doubles print as integers (jq's
/// computed-number style: `1+1` prints `2`, not `2.0`); everything else
/// uses serde_json's shortest-roundtrip formatting. Test harnesses must
/// serialize inputs with the same function so that jq 1.7's number-literal
/// preservation agrees with tjq's formatting.
pub fn canonical_number(n: f64) -> String {
    if n == n.trunc() && n.abs() < 1e17 && (n != 0.0 || n.is_sign_positive()) {
        format!("{}", n as i64)
    } else {
        // Non-finite numbers print as null in jq (NaN); infinities are
        // clamped upstream and should not reach here
        let text = serde_json::Number::from_f64(n)
            .map(|m| m.to_string())
            .unwrap_or_else(|| "null".to_string());
        // jq prints exponents as `E+308` / `E-324`; serde/ryu print
        // `e308` / `e-324`
        if let Some(pos) = text.find(['e', 'E']) {
            let (mantissa, exp) = text.split_at(pos);
            let exp = &exp[1..];
            if exp.starts_with('-') {
                format!("{mantissa}E{exp}")
            } else {
                format!("{mantissa}E+{exp}")
            }
        } else {
            text
        }
    }
}

impl Json {
    /// Compact RFC 8259 serialization, matching `jq -c` (and therefore
    /// `tostring`): proper key quoting and string escaping, no spaces.
    /// `Display` is looser (unquoted keys) and must not be used for
    /// interchange.
    pub fn to_compact_string(&self) -> String {
        fn escape(s: &str, out: &mut String) {
            out.push('"');
            for c in s.chars() {
                match c {
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    '\r' => out.push_str("\\r"),
                    '\t' => out.push_str("\\t"),
                    c if (c as u32) < 0x20 => {
                        out.push_str(&format!("\\u{:04x}", c as u32));
                    }
                    c => out.push(c),
                }
            }
            out.push('"');
        }
        fn go(j: &Json, out: &mut String) {
            match j {
                Json::Null => out.push_str("null"),
                Json::Boolean(b) => out.push_str(if *b { "true" } else { "false" }),
                Json::Number(n) => out.push_str(&canonical_number(*n)),
                Json::String(s) => escape(s, out),
                Json::Array(arr) => {
                    out.push('[');
                    for (i, v) in arr.iter().enumerate() {
                        if i != 0 {
                            out.push(',');
                        }
                        go(v, out);
                    }
                    out.push(']');
                }
                Json::Object(obj) => {
                    out.push('{');
                    for (i, (k, v)) in obj.iter().enumerate() {
                        if i != 0 {
                            out.push(',');
                        }
                        escape(k, out);
                        out.push(':');
                        go(v, out);
                    }
                    out.push('}');
                }
            }
        }
        let mut out = String::new();
        go(self, &mut out);
        out
    }

    pub fn debug(&self) -> String {
        match self {
            Json::Null => "null".to_string(),
            Json::Boolean(b) => format!("boolean ({})", b),
            Json::Number(n) => format!("number ({})", n),
            Json::String(s) => format!("string \"{}\"", s),
            Json::Array(vec) => format!(
                "array [{}]",
                vec.iter()
                    .map(|j| j.debug())
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Json::Object(vec) => format!(
                "object {{ {} }}",
                vec.iter()
                    .map(|(k, j)| format!("\"{}\": {}", k, j.debug()))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
        }
    }
}
