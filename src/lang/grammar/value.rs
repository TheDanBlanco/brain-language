use core::fmt;
use std::collections::BTreeMap;

use super::statements::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Number(i64),
    String(String),
    Boolean(bool),
    Collection(Vec<Value>),
    Function(Vec<String>, Box<Statement>),
    Map(BTreeMap<Value, Value>),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(string) => write!(f, "{string}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::Boolean(bool) => write!(f, "{bool}"),
            Value::Null => write!(f, "null"),
            Value::Function(_, _) => write!(f, "[function]"),
            Value::Map(map) => {
                let mut output = String::new();
                for (i, (key, value)) in map.iter().enumerate() {
                    output.push_str(&format!("{key}: {value}"));
                    if i != map.len() - 1 {
                        output.push_str(", ");
                    }
                }
                if output.is_empty() {
                    return write!(f, "{{}}");
                }
                write!(f, "{{ {output} }}")
            }
            Value::Collection(collection) => {
                let mut output = String::new();
                for (i, value) in collection.iter().enumerate() {
                    output.push_str(&value.to_string());
                    if i != collection.len() - 1 {
                        output.push_str(", ");
                    }
                }
                write!(f, "[{}]", output)
            }
        }
    }
}
