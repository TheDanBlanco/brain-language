use crate::lang::grammar::{context::Context, value::Value};

use super::{Evaluatable, Expression};

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
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut values = Vec::new();

        for element in &self.elements {
            let value = element.eval(context)?;
            values.push(value.clone());
        }

        Ok(Value::Collection(values))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_new_collection() {
        let collection = Collection::new(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);
        assert_eq!(
            collection.elements,
            vec![
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
                Expression::new_literal(Value::Number(3)),
            ]
        );
    }

    #[test]
    fn eval_collection() {
        let context = &mut Context::new();
        let collection = Collection::new(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);

        let result = collection.eval(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3),])
        );
    }
}
