use crate::lang::parser_new::{value::Value, context::Context};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    GreatherThanEqual,
    LessThan,
    LessThanEqual,
}

impl Comparison {
    pub fn eval(&self, left: Value, right: Value, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left, right) {
            (Comparison::Equal, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left == right)),
            (Comparison::NotEqual, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left != right)),
            (Comparison::GreaterThan, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left > right)),
            (Comparison::GreatherThanEqual, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left >= right)),
            (Comparison::LessThan, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left < right)),
            (Comparison::Equal, Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left <= right)),
            (Comparison::LessThanEqual, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left <= right)),
            (Comparison::Equal, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left == right)),
            (Comparison::NotEqual, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left != right)),
            (Comparison::GreaterThan, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left > right)),
            (Comparison::GreatherThanEqual, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left >= right)),
            (Comparison::LessThan, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left < right)),
            (Comparison::LessThanEqual, Value::String(left), Value::String(right)) => Ok(Value::Boolean(left <= right)),
            (Comparison::Equal, Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left == right)),
            (Comparison::NotEqual, Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left != right)),
            _ => Err("Invalid comparison operation".into()),
        }
    }
}
