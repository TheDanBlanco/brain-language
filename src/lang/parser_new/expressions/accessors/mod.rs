use crate::lang::parser_new::{context::Context, value::Value};

use self::{field::Field, index::Index};

use super::expression::Evaluatable;

pub mod field;
pub mod index;

pub enum Accessor {
    Index(Index),
    Field(Field),
}

impl Evaluatable for Accessor {
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Accessor::Index(index) => index.eval(context),
            Accessor::Field(field) => field.eval(context),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::parser_new::expressions::{expression::Expression, map::Map};

    use super::*;

    #[test]
    fn eval_index_accessor() {
        let context = &mut Context::new();
        let accessor = Accessor::Index(Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
        ));

        let result = accessor.eval(context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_field_accessor() {
        let context = &mut Context::new();
        let accessor = Accessor::Field(Field::new(
            Value::String("a".to_string()),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        ));

        let result = accessor.eval(context);

        assert!(result.is_ok());
    }
}
