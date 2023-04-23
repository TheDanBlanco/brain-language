use core::fmt;
use std::collections::BTreeMap;

use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::value::complex::ComplexValue;
use crate::grammar::value::literal::LiteralValue;

use self::complex::collection::Collection;
use self::complex::function::Function;
use self::complex::map::Map;

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
