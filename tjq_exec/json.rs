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
                println!("a1: {:?}, a2: {:?}", a1, a2);
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
                for ((k1, j1), (k2, j2)) in o1.iter().zip(o2.iter()) {
                    match k1.cmp(k2) {
                        std::cmp::Ordering::Equal => match j1.cmp(j2) {
                            std::cmp::Ordering::Equal => continue,
                            other => return other,
                        },
                        other => return other,
                    }
                }
                o1.len().cmp(&o2.len())
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
