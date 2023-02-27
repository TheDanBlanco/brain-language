use core::fmt;

use crate::lang::parser_new::{
    error::{Error, ErrorKind},
    value::Value,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    GreatherThanEqual,
    LessThan,
    LessThanEqual,
}

impl fmt::Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Comparison::Equal => write!(f, "equal"),
            Comparison::NotEqual => write!(f, "not equal"),
            Comparison::GreaterThan => write!(f, "greater than"),
            Comparison::GreatherThanEqual => write!(f, "greater than or equal to"),
            Comparison::LessThan => write!(f, "less than"),
            Comparison::LessThanEqual => write!(f, "less than or equal to"),
        }
    }
}

impl Comparison {
    pub fn eval(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left.clone(), right.clone()) {
            (Comparison::Equal, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left == right))
            }
            (Comparison::NotEqual, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left != right))
            }
            (Comparison::GreaterThan, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left > right))
            }
            (Comparison::GreatherThanEqual, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left >= right))
            }
            (Comparison::LessThan, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left < right))
            }
            (Comparison::LessThanEqual, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Boolean(left <= right))
            }
            (Comparison::Equal, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left == right))
            }
            (Comparison::NotEqual, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left != right))
            }
            (Comparison::GreaterThan, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left > right))
            }
            (Comparison::GreatherThanEqual, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left >= right))
            }
            (Comparison::LessThan, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left < right))
            }
            (Comparison::LessThanEqual, Value::String(left), Value::String(right)) => {
                Ok(Value::Boolean(left <= right))
            }
            (Comparison::Equal, Value::Boolean(left), Value::Boolean(right)) => {
                Ok(Value::Boolean(left == right))
            }
            (Comparison::NotEqual, Value::Boolean(left), Value::Boolean(right)) => {
                Ok(Value::Boolean(left != right))
            }
            _ => Err(Error::new(
                ErrorKind::InvalidComparisonOperation,
                format!("Cannot do {self} comparison on {left} and {right}"),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equal_numbers() {
        let result = Comparison::Equal.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_numbers() {
        let result = Comparison::NotEqual.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_numbers() {
        let result = Comparison::GreaterThan.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_or_equal_to_numbers() {
        let result = Comparison::GreatherThanEqual.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_numbers() {
        let result = Comparison::LessThan.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn less_than_or_equal_to_numbers() {
        let result = Comparison::LessThanEqual.eval(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn equal_strings() {
        let result = Comparison::Equal.eval(
            Value::String("a".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_strings() {
        let result = Comparison::NotEqual.eval(
            Value::String("a".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_strings() {
        let result = Comparison::GreaterThan.eval(
            Value::String("b".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn greater_than_equal_to_string() {
        let result = Comparison::GreatherThanEqual.eval(
            Value::String("b".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_strings() {
        let result = Comparison::LessThan.eval(
            Value::String("a".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_equal_to_strings() {
        let result = Comparison::LessThanEqual.eval(
            Value::String("b".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn equal_booleans() {
        let result = Comparison::Equal.eval(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_booleans() {
        let result = Comparison::NotEqual.eval(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn invalid_comparison() {
        let result = Comparison::Equal.eval(Value::Boolean(true), Value::Number(1));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidComparisonOperation]: Cannot do equal comparison on true and 1",
        )
    }
}
