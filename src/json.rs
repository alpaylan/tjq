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

impl PartialOrd for Json {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // JSON values are ordered as follows:
        // null
        // false
        // true
        // numbers
        // strings, in alphabetical order (by unicode codepoint value)
        // arrays, in lexical order
        // objects
        match (self, other) {
            (Json::Null, Json::Null) => Some(std::cmp::Ordering::Equal),
            (Json::Null, _) => Some(std::cmp::Ordering::Less),
            (_, Json::Null) => Some(std::cmp::Ordering::Greater),
            (Json::Boolean(b1), Json::Boolean(b2)) => Some(b1.cmp(b2)),
            (Json::Boolean(_), _) => Some(std::cmp::Ordering::Less),
            (_, Json::Boolean(_)) => Some(std::cmp::Ordering::Greater),
            (Json::Number(n1), Json::Number(n2)) => n1.partial_cmp(n2),
            (Json::Number(_), _) => Some(std::cmp::Ordering::Less),
            (_, Json::Number(_)) => Some(std::cmp::Ordering::Greater),
            (Json::String(s1), Json::String(s2)) => s1.partial_cmp(s2),
            (Json::String(_), _) => Some(std::cmp::Ordering::Less),
            (_, Json::String(_)) => Some(std::cmp::Ordering::Greater),
            (Json::Array(a1), Json::Array(a2)) => {
                for (j1, j2) in a1.iter().zip(a2.iter()) {
                    match j1.partial_cmp(j2) {
                        Some(std::cmp::Ordering::Equal) => continue,
                        other => return other,
                    }
                }
                a1.len().partial_cmp(&a2.len())
            }
            (Json::Array(_), _) => Some(std::cmp::Ordering::Less),
            (_, Json::Array(_)) => Some(std::cmp::Ordering::Greater),
            (Json::Object(o1), Json::Object(o2)) => {
                for ((k1, j1), (k2, j2)) in o1.iter().zip(o2.iter()) {
                    match k1.cmp(k2) {
                        std::cmp::Ordering::Equal => match j1.partial_cmp(j2) {
                            Some(std::cmp::Ordering::Equal) => continue,
                            other => return other,
                        },
                        other => return Some(other),
                    }
                }
                o1.len().partial_cmp(&o2.len())
            }
        }
    }
}

impl Eq for Json {}
impl Ord for Json {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
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

impl Json {
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
