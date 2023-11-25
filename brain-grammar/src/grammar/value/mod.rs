use core::fmt;
use std::collections::BTreeMap;

use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::value::complex::ComplexValue;
use crate::grammar::value::literal::LiteralValue;

use self::complex::collection::Collection;
use self::complex::enumdefinition::EnumDefinition;
use self::complex::function::Function;
use self::complex::map::Map;
use self::complex::r#enum::Enum;
use self::complex::tuple::Tuple;

use super::statements::Statement;
use super::token::BrainToken;
use super::{Match, Parse};

pub mod complex;
pub mod literal;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Value {
    Literal(LiteralValue),
    Complex(ComplexValue),
}

impl Value {
    pub fn new_string(string: String) -> Self {
        Value::Literal(LiteralValue::String(string))
    }

    pub fn new_number(number: i64) -> Self {
        Value::Literal(LiteralValue::Number(number))
    }

    pub fn new_boolean(boolean: bool) -> Self {
        Value::Literal(LiteralValue::Boolean(boolean))
    }

    pub fn new_null() -> Self {
        Value::Literal(LiteralValue::Null)
    }

    pub fn new_map(map: BTreeMap<Value, Value>) -> Self {
        Value::Complex(ComplexValue::Map(Map::new(map)))
    }

    pub fn new_collection(collection: Vec<Value>) -> Self {
        Value::Complex(ComplexValue::Collection(Collection::new(collection)))
    }

    pub fn new_function(args: Vec<String>, body: Statement) -> Self {
        Value::Complex(ComplexValue::Function(Function::new(args, body)))
    }

    pub fn new_enum_variant(name: String, variant: String) -> Self {
        Value::Complex(ComplexValue::Enum(Enum::new(name, variant)))
    }

    pub fn new_enum_definition(name: String, variants: Vec<String>) -> Self {
        Value::Complex(ComplexValue::EnumDefinition(EnumDefinition::new(
            name, variants,
        )))
    }

    pub fn new_tuple(tuple: Vec<Value>) -> Self {
        Value::Complex(ComplexValue::Tuple(Tuple::new(tuple)))
    }
}

impl Parse for Value {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let token = stream.assert_peek("Expected token, found End of File".to_string())?;

        if LiteralValue::matches(&token.token) {
            return Ok(Value::Literal(LiteralValue::parse(stream)?));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!(
                "Expected value, found {} ({} - {})",
                token.token, token.span.start, token.span.end
            ),
        ));
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Literal(literal) => write!(f, "{}", literal),
            Value::Complex(complex) => write!(f, "{}", complex),
        }
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn test_literal() {
        let tokens = vec![Token::new(0..2, BrainToken::Number, "10".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let value = Value::parse(&mut stream);

        assert!(value.is_ok());
    }

    #[test]
    fn test_complex() {
        let tokens = vec![Token::new(0..2, BrainToken::Identifier, "map".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let value = Value::parse(&mut stream);

        assert!(value.is_err());
    }

    #[test]
    fn test_display_literal() {
        let value = Value::new_string("Hello World".to_string());
        assert_eq!(value.to_string(), "Hello World".to_string());
    }

    #[test]
    fn test_display_complex() {
        let value = Value::new_enum_variant("test".to_string(), "one".to_string());
        assert_eq!(value.to_string(), "test::one".to_string());
    }
}
