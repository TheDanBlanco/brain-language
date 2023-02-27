use crate::lang::parser_new::{value::Value, context::Context, error::{Error, ErrorKind}};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Logical {
    And,
    Or,
}

impl Logical {
    pub fn eval(&self, left: Value, right: Value, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left, right) {
            (Logical::And, Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left && right)),
            (Logical::Or, Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left || right)),
            _ => Err(Error::new(
                ErrorKind::InvalidLogicalOperation,
                "Invalid logical operation".to_string(),
            )),
        }
    }
}

