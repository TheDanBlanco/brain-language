use std::error::Error;

use crate::lang::parser_new::{value::Value, context::Context};

use super::expression::Evaluatable;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Literal {
    value: Value,
}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal { value }
    }
}

impl Evaluatable for Literal {
    fn eval<'a>(&'a self, _context: &mut Context) -> Result<&Value, Box<dyn std::error::Error>> {
        Ok(&self.value.clone())
    }
}