use core::fmt;

use brain_error::{Error, ErrorKind};

use crate::grammar::{
    token::BrainToken,
    value::{literal::LiteralValue, Value},
    Match,
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
            (
                Comparison::Equal,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left == right)),
            (
                Comparison::NotEqual,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left != right)),
            (
                Comparison::GreaterThan,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left > right)),
            (
                Comparison::GreaterThanEqual,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left >= right)),
            (
                Comparison::LessThan,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left < right)),
            (
                Comparison::LessThanEqual,
                Value::Literal(LiteralValue::Number(left)),
                Value::Literal(LiteralValue::Number(right)),
            ) => Ok(Value::new_boolean(left <= right)),
            (
                Comparison::Equal,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left == right)),
            (
                Comparison::NotEqual,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left != right)),
            (
                Comparison::GreaterThan,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left > right)),
            (
                Comparison::GreaterThanEqual,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left >= right)),
            (
                Comparison::LessThan,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left < right)),
            (
                Comparison::LessThanEqual,
                Value::Literal(LiteralValue::String(left)),
                Value::Literal(LiteralValue::String(right)),
            ) => Ok(Value::new_boolean(left <= right)),
            (
                Comparison::Equal,
                Value::Literal(LiteralValue::Boolean(left)),
                Value::Literal(LiteralValue::Boolean(right)),
            ) => Ok(Value::new_boolean(left == right)),
            (
                Comparison::NotEqual,
                Value::Literal(LiteralValue::Boolean(left)),
                Value::Literal(LiteralValue::Boolean(right)),
            ) => Ok(Value::new_boolean(left != right)),
            _ => Err(Error::new(
                ErrorKind::InvalidComparisonOperation,
                format!("Cannot do {self} comparison on {left} and {right}"),
            )),
        }
    }

    pub fn parse(token: &BrainToken) -> Self {
        match token {
            BrainToken::Equal => Comparison::Equal,
            BrainToken::NotEqual => Comparison::NotEqual,
            BrainToken::GreaterThan => Comparison::GreaterThan,
            BrainToken::GreaterThanEqual => Comparison::GreaterThanEqual,
            BrainToken::LessThan => Comparison::LessThan,
            BrainToken::LessThanEqual => Comparison::LessThanEqual,
            _ => unreachable!(),
        }
    }
}

impl Match for Comparison {
    fn matches(token: &BrainToken) -> bool {
        matches!(
            token,
            BrainToken::Equal
                | BrainToken::NotEqual
                | BrainToken::GreaterThan
                | BrainToken::GreaterThanEqual
                | BrainToken::LessThan
                | BrainToken::LessThanEqual
        )
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn equal_numbers() {
        let result = Comparison::Equal.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn not_equal_numbers() {
        let result = Comparison::NotEqual.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(false));
    }

    #[test]
    fn greater_than_numbers() {
        let result = Comparison::GreaterThan.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(false));
    }

    #[test]
    fn greater_than_or_equal_to_numbers() {
        let result =
            Comparison::GreaterThanEqual.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn less_than_numbers() {
        let result = Comparison::LessThan.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(false));
    }

    #[test]
    fn less_than_or_equal_to_numbers() {
        let result = Comparison::LessThanEqual.evaluate(Value::new_number(1), Value::new_number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn equal_strings() {
        let result = Comparison::Equal.evaluate(
            Value::new_string("a".to_string()),
            Value::new_string("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn not_equal_strings() {
        let result = Comparison::NotEqual.evaluate(
            Value::new_string("a".to_string()),
            Value::new_string("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(false));
    }

    #[test]
    fn greater_than_strings() {
        let result = Comparison::GreaterThan.evaluate(
            Value::new_string("b".to_string()),
            Value::new_string("a".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn greater_than_equal_to_string() {
        let result = Comparison::GreaterThanEqual.evaluate(
            Value::new_string("b".to_string()),
            Value::new_string("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn less_than_strings() {
        let result = Comparison::LessThan.evaluate(
            Value::new_string("a".to_string()),
            Value::new_string("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn less_than_equal_to_strings() {
        let result = Comparison::LessThanEqual.evaluate(
            Value::new_string("b".to_string()),
            Value::new_string("b".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn equal_booleans() {
        let result = Comparison::Equal.evaluate(Value::new_boolean(true), Value::new_boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn not_equal_booleans() {
        let result =
            Comparison::NotEqual.evaluate(Value::new_boolean(true), Value::new_boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(false));
    }

    #[test]
    fn invalid_comparison() {
        let result = Comparison::Equal.evaluate(Value::new_boolean(true), Value::new_number(1));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidComparisonOperation]: Cannot do equal comparison on true and 1",
        )
    }

    #[test]
    fn matches_comparison() {
        assert!(Comparison::matches(&BrainToken::Equal));
        assert!(Comparison::matches(&BrainToken::NotEqual));
        assert!(Comparison::matches(&BrainToken::GreaterThan));
        assert!(Comparison::matches(&BrainToken::GreaterThanEqual));
        assert!(Comparison::matches(&BrainToken::LessThan));
        assert!(Comparison::matches(&BrainToken::LessThanEqual));

        assert!(!Comparison::matches(&BrainToken::Plus))
    }

    #[test]
    fn parse_comparison() {
        assert_eq!(Comparison::parse(&BrainToken::Equal), Comparison::Equal);
        assert_eq!(
            Comparison::parse(&BrainToken::NotEqual),
            Comparison::NotEqual
        );
        assert_eq!(
            Comparison::parse(&BrainToken::GreaterThan),
            Comparison::GreaterThan
        );
        assert_eq!(
            Comparison::parse(&BrainToken::GreaterThanEqual),
            Comparison::GreaterThanEqual
        );
        assert_eq!(
            Comparison::parse(&BrainToken::LessThan),
            Comparison::LessThan
        );
        assert_eq!(
            Comparison::parse(&BrainToken::LessThanEqual),
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
        Comparison::parse(&BrainToken::Plus);
    }
}
