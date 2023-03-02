use crate::lang::grammar::{context::Context, value::Value};

use self::{field::Field, index::Index};

use super::{Evaluatable, Expression};

pub mod field;
pub mod index;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessor {
    Index(Index),
    Field(Field),
}

impl Accessor {
    pub fn new_index(index: Expression, target: Expression) -> Self {
        Accessor::Index(Index::new(index, target))
    }

    pub fn new_field(field: Value, target: Expression) -> Self {
        Accessor::Field(Field::new(field, target))
    }
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
    use crate::lang::grammar::expressions::{map::Map, Expression};

    use super::*;

    #[test]
    fn new_index() {
        let index = Expression::new_literal(Value::Number(0));
        let target = Expression::new_collection(vec![]);

        let accessor = Accessor::new_index(index.clone(), target.clone());

        assert_eq!(accessor, Accessor::Index(Index::new(index, target)));
    }

    #[test]
    fn new_field() {
        let field = Value::Number(0);
        let target = Expression::new_collection(vec![]);

        let accessor = Accessor::new_field(field.clone(), target.clone());

        assert_eq!(accessor, Accessor::Field(Field::new(field, target)));
    }

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
