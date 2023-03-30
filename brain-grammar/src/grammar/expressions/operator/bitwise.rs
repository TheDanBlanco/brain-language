use core::fmt;

use brain_error::{Error, ErrorKind};

use crate::grammar::{token::BrainToken, value::Value, Match};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bitwise {
    And,
    Or,
}

impl fmt::Display for Bitwise {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Bitwise::And => write!(f, "bitwise AND"),
            Bitwise::Or => write!(f, "bitwise OR"),
        }
    }
}

impl Bitwise {
    pub fn evaluate(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left.clone(), right.clone()) {
            (Bitwise::And, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Number(left & right))
            }
            (Bitwise::Or, Value::Number(left), Value::Number(right)) => {
                Ok(Value::Number(left | right))
            }
            _ => Err(Error::new(
                ErrorKind::InvalidBitwiseOperation,
                format!("Cannot do {self} operation on {left} and {right}"),
            )),
        }
    }

    pub fn parse(token: &BrainToken) -> Self {
        match token {
            BrainToken::BitwiseAnd => Bitwise::And,
            BrainToken::BitwiseOr => Bitwise::Or,
            _ => unreachable!(),
        }
    }
}

impl Match for Bitwise {
    fn matches(token: &BrainToken) -> bool {
        matches!(token, BrainToken::BitwiseAnd | BrainToken::BitwiseOr)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn bitwise_and() {
        let result = Bitwise::And.evaluate(Value::Number(5), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn bitwise_or() {
        let result = Bitwise::Or.evaluate(Value::Number(5), Value::Number(1));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(5));
    }

    #[test]
    fn matches_bitwise() {
        assert!(Bitwise::matches(&BrainToken::BitwiseAnd));
        assert!(Bitwise::matches(&BrainToken::BitwiseOr));
    }

    #[test]
    fn invalid_bitwise_and_operation() {
        let result = Bitwise::And.evaluate(Value::Boolean(true), Value::Number(1));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidBitwiseOperation]: Cannot do bitwise AND operation on true and 1",
        )
    }

    #[test]
    fn invalid_bitwise_or_operation() {
        let result = Bitwise::Or.evaluate(Value::Boolean(true), Value::Number(1));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidBitwiseOperation]: Cannot do bitwise OR operation on true and 1",
        )
    }

    #[test]
    fn parse_bitwise() {
        assert_eq!(Bitwise::parse(&BrainToken::BitwiseAnd), Bitwise::And);
        assert_eq!(Bitwise::parse(&BrainToken::BitwiseOr), Bitwise::Or);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Bitwise::And), "bitwise AND");
        assert_eq!(format!("{}", Bitwise::Or), "bitwise OR");
    }

    #[test]
    #[should_panic]
    fn parse_bitwise_unreachable() {
        Bitwise::parse(&BrainToken::Plus);
    }
}
