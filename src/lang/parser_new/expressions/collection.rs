use crate::lang::parser_new::{value::Value, context::Context};

use super::expression::{Evaluatable, Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Collection {
    elements: Vec<Expression>,
}

impl Collection { 
    pub fn new(elements: Vec<Expression>) -> Self {
        Collection { elements }
    }
}

impl Evaluatable for Collection {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut values = Vec::new();

        for element in &self.elements {
            let value = element.eval(context)?;
            values.push(value.clone());
        }

        Ok(Value::Collection(values))
    }
}