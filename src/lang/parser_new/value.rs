use core::fmt;
use std::collections::BTreeMap;

use super::expressions::expression::Expression;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Number(i64),
    String(String),
    Boolean(bool),
    Collection(Vec<Value>),
    Function(Box<Expression>, Vec<String>),
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
                if map.len() == 0 {
                    return write!(f, "{{empty map}}");
                }
                for (_i, (key, value)) in map.iter().enumerate() {
                    output.push_str(&format!("{key}: {value}\n"));
                }
                write!(f, "{{\n{output}}}")
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

impl Value {
    pub fn new_number(number: i64) -> Self {
        Value::Number(number)
    }

    pub fn new_string(string: String) -> Self {
        Value::String(string)
    }

    pub fn new_boolean(bool: bool) -> Self {
        Value::Boolean(bool)
    }


    pub fn new_collection(collection: Vec<Value>) -> Self {
        Value::Collection(collection)
    }

    pub fn new_function(expression: Expression, args: Vec<String>) -> Self {
        Value::Function(Box::new(expression), args)
    }

    pub fn new_map(map: BTreeMap<Value, Value>) -> Self {
        Value::Map(map)
    }

    pub fn new_null() -> Self {
        Value::Null
    }
}