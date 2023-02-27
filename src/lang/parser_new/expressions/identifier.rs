use crate::lang::parser_new::{value::Value, context::Context, error::{ErrorKind, Error}};

use super::expression::Evaluatable;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Identifier { name }
    }
}

impl Evaluatable for Identifier {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        if let Some(value) = context.symbols.get(&self.name) {
            return Ok(value.clone());
        }

        return Err(Error::new(
            ErrorKind::UnknownIdentifier,
            format!("Unknown identifier: {}", self.name),
        ));
    }
}