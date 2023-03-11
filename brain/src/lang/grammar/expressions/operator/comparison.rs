use core::fmt;

use crate::lang::{
    grammar::{
        error::{Error, ErrorKind},
        value::Value,
        Match,
    },
    tokens::tokenkind::TokenKind,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

impl fmt::Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Comparison::Equal => write!(f, "equal"),
            Comparison::NotEqual => write!(f, "not equal"),
            Comparison::GreaterThan => write!(f, "greater than"),
            Comparison::GreaterThanEqual => write!(f, "greater than or equal to"),
            Comparison::LessThan => write!(f, "less than"),
            Comparison::LessThanEqual => write!(f, "less than or equal to"),
        }
    }
}

impl Comparison {
    pub fn evaluate(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
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
            (Comparison::GreaterThanEqual, Value::Number(left), Value::Number(right)) => {
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
            (Comparison::GreaterThanEqual, Value::String(left), Value::String(right)) => {
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

    pub fn parse(token: &TokenKind) -> Self {
        match token {
            TokenKind::Equal => Comparison::Equal,
            TokenKind::NotEqual => Comparison::NotEqual,
            TokenKind::GreaterThan => Comparison::GreaterThan,
            TokenKind::GreaterThanEqual => Comparison::GreaterThanEqual,
            TokenKind::LessThan => Comparison::LessThan,
            TokenKind::LessThanEqual => Comparison::LessThanEqual,
            _ => unreachable!(),
        }
    }
}

impl Match for Comparison {
    fn matches(token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::GreaterThan
                | TokenKind::GreaterThanEqual
                | TokenKind::LessThan
                | TokenKind::LessThanEqual
        )
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn equal_numbers() {
        let result = Comparison::Equal.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_numbers() {
        let result = Comparison::NotEqual.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_numbers() {
        let result = Comparison::GreaterThan.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_or_equal_to_numbers() {
        let result = Comparison::GreaterThanEqual.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_numbers() {
        let result = Comparison::LessThan.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn less_than_or_equal_to_numbers() {
        let result = Comparison::LessThanEqual.evaluate(Value::Number(1), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn equal_strings() {
        let result = Comparison::Equal.evaluate(
            Value::String("a".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_strings() {
        let result = Comparison::NotEqual.evaluate(
            Value::String("a".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn greater_than_strings() {
        let result = Comparison::GreaterThan.evaluate(
            Value::String("b".to_string()),
            Value::String("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn greater_than_equal_to_string() {
        let result = Comparison::GreaterThanEqual.evaluate(
            Value::String("b".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_strings() {
        let result = Comparison::LessThan.evaluate(
            Value::String("a".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn less_than_equal_to_strings() {
        let result = Comparison::LessThanEqual.evaluate(
            Value::String("b".to_string()),
            Value::String("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn equal_booleans() {
        let result = Comparison::Equal.evaluate(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn not_equal_booleans() {
        let result = Comparison::NotEqual.evaluate(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn invalid_comparison() {
        let result = Comparison::Equal.evaluate(Value::Boolean(true), Value::Number(1));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidComparisonOperation]: Cannot do equal comparison on true and 1",
        )
    }

    #[test]
    fn matches_comparison() {
        assert!(Comparison::matches(&TokenKind::Equal));
        assert!(Comparison::matches(&TokenKind::NotEqual));
        assert!(Comparison::matches(&TokenKind::GreaterThan));
        assert!(Comparison::matches(&TokenKind::GreaterThanEqual));
        assert!(Comparison::matches(&TokenKind::LessThan));
        assert!(Comparison::matches(&TokenKind::LessThanEqual));

        assert!(!Comparison::matches(&TokenKind::Add))
    }

    #[test]
    fn parse_comparison() {
        assert_eq!(Comparison::parse(&TokenKind::Equal), Comparison::Equal);
        assert_eq!(
            Comparison::parse(&TokenKind::NotEqual),
            Comparison::NotEqual
        );
        assert_eq!(
            Comparison::parse(&TokenKind::GreaterThan),
            Comparison::GreaterThan
        );
        assert_eq!(
            Comparison::parse(&TokenKind::GreaterThanEqual),
            Comparison::GreaterThanEqual
        );
        assert_eq!(
            Comparison::parse(&TokenKind::LessThan),
            Comparison::LessThan
        );
        assert_eq!(
            Comparison::parse(&TokenKind::LessThanEqual),
            Comparison::LessThanEqual
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Comparison::Equal), "equal");
        assert_eq!(format!("{}", Comparison::NotEqual), "not equal");
        assert_eq!(format!("{}", Comparison::GreaterThan), "greater than");
        assert_eq!(
            format!("{}", Comparison::GreaterThanEqual),
            "greater than or equal to"
        );
        assert_eq!(format!("{}", Comparison::LessThan), "less than");
        assert_eq!(
            format!("{}", Comparison::LessThanEqual),
            "less than or equal to"
        );
    }

    #[test]
    #[should_panic]
    fn parse_comparison_unreachable() {
        Comparison::parse(&TokenKind::Add);
    }
}
