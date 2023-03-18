use brain_error::{Error, ErrorKind};

use crate::lang::grammar::{token::BrainToken, value::Value, Match};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Logical {
    And,
    Or,
}

impl Logical {
    pub fn evaluate(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left.clone(), right.clone()) {
            (Logical::And, Value::Boolean(left), Value::Boolean(right)) => {
                Ok(Value::Boolean(left && right))
            }
            (Logical::Or, Value::Boolean(left), Value::Boolean(right)) => {
                Ok(Value::Boolean(left || right))
            }
            _ => Err(Error::new(
                ErrorKind::InvalidLogicalOperation,
                // print a better error message
                format!("cannot do logical comparison on {left} and {right}"),
            )),
        }
    }

    pub fn parse(token: &BrainToken) -> Self {
        match token {
            BrainToken::And => Logical::And,
            BrainToken::Or => Logical::Or,
            _ => unreachable!(),
        }
    }
}

impl Match for Logical {
    fn matches(token: &BrainToken) -> bool {
        matches!(token, BrainToken::And | BrainToken::Or)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluate_and() {
        let result = Logical::And.evaluate(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn evaluate_or() {
        let result = Logical::Or.evaluate(Value::Boolean(true), Value::Boolean(false));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn evaluate_or_both_false() {
        let result = Logical::Or.evaluate(Value::Boolean(false), Value::Boolean(false));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(false));
    }

    #[test]
    fn evaluate_or_both_true() {
        let result = Logical::Or.evaluate(Value::Boolean(true), Value::Boolean(true));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn evaluate_with_invalid_value() {
        let result = Logical::Or.evaluate(Value::Number(1), Value::Boolean(true));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidLogicalOperation]: cannot do logical comparison on 1 and true".to_string()
        )
    }

    #[test]
    fn matches_logical() {
        assert!(Logical::matches(&BrainToken::And));
        assert!(Logical::matches(&BrainToken::Or));
    }

    #[test]
    fn parse_logical() {
        assert_eq!(Logical::parse(&BrainToken::And), Logical::And);
        assert_eq!(Logical::parse(&BrainToken::Or), Logical::Or);
    }

    #[test]
    #[should_panic]
    fn parse_comparison_unreachable() {
        Logical::parse(&BrainToken::Plus);
    }
}
