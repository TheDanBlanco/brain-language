use core::fmt;

use brain_error::{Error, ErrorKind};

use crate::grammar::{
    token::BrainToken,
    value::{literal::LiteralValue, Value},
    Match,
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
    pub fn evaluate(&self, left: Value, right: Value) -> Result<Value, Box<dyn std::error::Error>> {
        match (self, left.clone(), right.clone()) {
            (
                Mathematical::Add,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_number(lhs + rhs)),
            (
                Mathematical::Add,
                Value::Literal(LiteralValue::String(lhs)),
                Value::Literal(LiteralValue::String(rhs)),
            ) => Ok(Value::new_string(format!("{lhs}{rhs}"))),
            (
                Mathematical::Multiply,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_number(lhs * rhs)),
            (
                Mathematical::Multiply,
                Value::Literal(LiteralValue::String(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_string(lhs.repeat(rhs as usize))),
            (
                Mathematical::Multiply,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::String(rhs)),
            ) => Ok(Value::new_string(rhs.repeat(lhs as usize))),
            (
                Mathematical::Subtract,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_number(lhs - rhs)),
            (
                Mathematical::Divide,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_number(lhs / rhs)),
            (
                Mathematical::Modulo,
                Value::Literal(LiteralValue::Number(lhs)),
                Value::Literal(LiteralValue::Number(rhs)),
            ) => Ok(Value::new_number(lhs % rhs)),
            _ => Err(Error::new(
                ErrorKind::InvalidMathematicalOperation,
                format!(
                    "cannot {} {left} and {right}",
                    self.to_string().to_lowercase()
                ),
            )),
        }
    }

    pub fn parse(token: &BrainToken) -> Self {
        match token {
            BrainToken::Plus => Mathematical::Add,
            BrainToken::Minus => Mathematical::Subtract,
            BrainToken::Times => Mathematical::Multiply,
            BrainToken::Divide => Mathematical::Divide,
            BrainToken::Modulo => Mathematical::Modulo,
            _ => unreachable!(),
        }
    }
}

impl Match for Mathematical {
    fn matches(token: &BrainToken) -> bool {
        matches!(
            token,
            BrainToken::Plus
                | BrainToken::Minus
                | BrainToken::Times
                | BrainToken::Divide
                | BrainToken::Modulo
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::value::Value;

    #[test]
    fn add_numbers() {
        let result = Mathematical::Add.evaluate(Value::new_number(1), Value::new_number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(3));
    }

    #[test]
    fn add_strings() {
        let result = Mathematical::Add.evaluate(
            Value::new_string("hello".to_string()),
            Value::new_string("world".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_string("helloworld".to_string()));
    }

    #[test]
    fn multiply_numbers() {
        let result = Mathematical::Multiply.evaluate(Value::new_number(1), Value::new_number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(2));
    }

    #[test]
    fn multiply_strings() {
        let result = Mathematical::Multiply
            .evaluate(Value::new_string("hello".to_string()), Value::new_number(4));
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_string("hellohellohellohello".to_string())
        );
    }

    #[test]
    fn multiply_strings_left_hand_number() {
        let result = Mathematical::Multiply
            .evaluate(Value::new_number(4), Value::new_string("hello".to_string()));
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_string("hellohellohellohello".to_string())
        );
    }

    #[test]
    fn subtract_numbers() {
        let result = Mathematical::Subtract.evaluate(Value::new_number(1), Value::new_number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(-1));
    }

    #[test]
    fn divide_numbers() {
        let result = Mathematical::Divide.evaluate(Value::new_number(10), Value::new_number(2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(5));
    }

    #[test]
    fn modulo_numbers() {
        let result = Mathematical::Modulo.evaluate(Value::new_number(10), Value::new_number(3));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn invalid_operation() {
        let result = Mathematical::Add
            .evaluate(Value::new_number(1), Value::new_string("hello".to_string()));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMathematicalOperation]: cannot add 1 and hello"
        );
    }

    #[test]
    fn matches_comparison() {
        assert!(Mathematical::matches(&BrainToken::Plus));
        assert!(Mathematical::matches(&BrainToken::Minus));
        assert!(Mathematical::matches(&BrainToken::Times));
        assert!(Mathematical::matches(&BrainToken::Divide));
        assert!(Mathematical::matches(&BrainToken::Modulo));
    }

    #[test]
    fn parse_comparison() {
        assert_eq!(Mathematical::parse(&BrainToken::Plus), Mathematical::Add);
        assert_eq!(
            Mathematical::parse(&BrainToken::Minus),
            Mathematical::Subtract
        );
        assert_eq!(
            Mathematical::parse(&BrainToken::Times),
            Mathematical::Multiply
        );
        assert_eq!(
            Mathematical::parse(&BrainToken::Divide),
            Mathematical::Divide
        );
        assert_eq!(
            Mathematical::parse(&BrainToken::Modulo),
            Mathematical::Modulo
        );
    }

    #[test]
    #[should_panic]
    fn parse_comparison_unreachable() {
        Mathematical::parse(&BrainToken::And);
    }
}
