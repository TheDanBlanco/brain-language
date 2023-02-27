use core::fmt;

use crate::lang::parser_new::{
    error::{Error, ErrorKind},
    value::Value,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mathematical {
    Add,
    Multiply,
    Subtract,
    Divide,
    Modulo,
}

impl fmt::Display for Mathematical {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl Mathematical {
    pub fn eval<'a>(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left.clone(), right.clone()) {
            (Mathematical::Add, Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(lhs + rhs))
            }
            (Mathematical::Add, Value::String(lhs), Value::String(rhs)) => {
                Ok(Value::String(format!("{lhs}{rhs}")))
            }
            (Mathematical::Multiply, Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(lhs * rhs))
            }
            (Mathematical::Multiply, Value::String(lhs), Value::Number(rhs)) => {
                Ok(Value::String(lhs.repeat(rhs as usize)))
            }
            (Mathematical::Multiply, Value::Number(lhs), Value::String(rhs)) => {
                Ok(Value::String(rhs.repeat(lhs as usize)))
            }
            (Mathematical::Subtract, Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(lhs - rhs))
            }
            (Mathematical::Divide, Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(lhs / rhs))
            }
            (Mathematical::Modulo, Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(lhs % rhs))
            }
            _ => Err(Error::new(
                ErrorKind::InvalidMathematicalOperation,
                format!(
                    "cannot {} {left} and {right}",
                    self.to_string().to_lowercase()
                ),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::parser_new::value::Value;

    #[test]
    fn add_numbers() {
        let result = Mathematical::Add.eval(Value::Number(1), Value::Number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(3));
    }

    #[test]
    fn add_strings() {
        let result = Mathematical::Add.eval(
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::String("helloworld".to_string()));
    }

    #[test]
    fn multiply_numbers() {
        let result = Mathematical::Multiply.eval(Value::Number(1), Value::Number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(2));
    }

    #[test]
    fn multiply_strings() {
        let result =
            Mathematical::Multiply.eval(Value::String("hello".to_string()), Value::Number(4));
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::String("hellohellohellohello".to_string())
        );
    }

    #[test]
    fn multiply_strings_left_hand_number() {
        let result =
            Mathematical::Multiply.eval(Value::Number(4), Value::String("hello".to_string()));
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::String("hellohellohellohello".to_string())
        );
    }

    #[test]
    fn subtract_numbers() {
        let result = Mathematical::Subtract.eval(Value::Number(1), Value::Number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(-1));
    }

    #[test]
    fn divide_numbers() {
        let result = Mathematical::Divide.eval(Value::Number(10), Value::Number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(5));
    }

    #[test]
    fn modulo_numbers() {
        let result = Mathematical::Modulo.eval(Value::Number(10), Value::Number(3));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn invalid_operation() {
        let result = Mathematical::Add.eval(Value::Number(1), Value::String("hello".to_string()));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMathematicalOperation]: cannot add 1 and hello"
        );
    }
}
