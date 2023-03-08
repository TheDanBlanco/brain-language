use crate::lang::{
    grammar::{
        error::{Error, ErrorKind},
        value::Value,
        Match,
    },
    tokens::tokenkind::TokenKind,
};

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

    pub fn parse(token: &TokenKind) -> Self {
        match token {
            TokenKind::And => Logical::And,
            TokenKind::Or => Logical::Or,
            _ => unreachable!(),
        }
    }
}

impl Match for Logical {
    fn matches(token: &TokenKind) -> bool {
        matches!(token, TokenKind::And | TokenKind::Or)
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
}
