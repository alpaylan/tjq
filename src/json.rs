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
