use crate::lang::parser_new::{value::Value, context::Context};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mathematical {
    Add,
    Multiply,
    Subtract,
    Divide,
    Modulo,
}

impl Mathematical {
    pub fn eval<'a>(&self, left: & Value, right: & Value, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left, right) {
            (Mathematical::Add, Value::Number(lhs), Value::Number(rhs)) => Ok(&Value::Number(lhs + rhs)),
            (Mathematical::Add, Value::String(lhs), Value::String(rhs)) => Ok(&Value::String(format!("{lhs}{rhs}"))),
            (Mathematical::Multiply, Value::Number(lhs), Value::Number(rhs)) => Ok(&Value::Number(lhs * rhs)),
            (Mathematical::Multiply, Value::String(lhs), Value::Number(rhs)) => Ok(&Value::String(lhs.repeat(*rhs as usize))),
            (Mathematical::Multiply, Value::Number(lhs), Value::String(rhs)) => Ok(&Value::String(rhs.repeat(*lhs as usize))),
            (Mathematical::Subtract, Value::Number(lhs), Value::Number(rhs)) => Ok(&Value::Number(lhs - rhs)),
            (Mathematical::Divide, Value::Number(lhs), Value::Number(rhs)) => Ok(&Value::Number(lhs / rhs)),
            (Mathematical::Modulo, Value::Number(lhs), Value::Number(rhs)) => Ok(&Value::Number(lhs % rhs)),
            _ => Err("Invalid mathematical operation".into()),
        }
    }
}

