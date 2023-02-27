use crate::lang::parser_new::{
    context::Context,
    error::{Error, ErrorKind},
    expressions::expression::{Evaluatable, Expression},
    value::Value,
};

pub struct Field {
    field: Value,
    target: Box<Expression>,
}

impl Field {
    pub fn new(field: Value, target: Expression) -> Self {
        Field {
            field,
            target: Box::new(target),
        }
    }
}

impl Evaluatable for Field {
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let target = self.target.eval(context)?;

        if let Value::String(_) = &self.field {
        } else {
            return Err(Error::new(
                ErrorKind::InvalidType,
                format!("Field accessor '{}' must be of type String", self.field),
            ));
        }

        match &target {
            Value::Map(map) => match map.get(&self.field) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::new(
                    ErrorKind::KeyNotFound,
                    format!("'{}' in {target}", self.field),
                )),
            },
            _ => Err(Error::new(
                ErrorKind::InvalidType,
                format!("Cannot access field {} of {target}", self.field),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::lang::parser_new::expressions::map::Map;

    use super::*;

    #[test]
    fn create_new_field() {
        let field = Field::new(
            Value::String("a".to_string()),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );
        assert_eq!(field.field, Value::String("a".to_string()));
        assert_eq!(
            field.target,
            Box::new(Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )],)))
        );
    }

    #[test]
    fn field_access_map() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::String("a".to_string()),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn field_access_map_key_not_found() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::String("b".to_string()),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[KeyNotFound]: 'b' in { a: 1 }".to_string()
        );
    }

    #[test]
    fn field_access_target_not_map() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::String("a".to_string()),
            Expression::new_literal(Value::Number(1)),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Cannot access field a of 1".to_string()
        );
    }

    #[test]
    fn field_access_field_number() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::Number(1),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor '1' must be of type String".to_string()
        );
    }

    #[test]
    fn field_access_field_bool() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::Boolean(true),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor 'true' must be of type String".to_string()
        );
    }

    #[test]
    fn field_access_field_collection() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::Collection(vec![Value::Number(1)]),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor '[1]' must be of type String".to_string()
        );
    }

    #[test]
    fn field_access_field_map() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::Map(BTreeMap::new()),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor '{}' must be of type String".to_string()
        );
    }

    #[test]
    fn field_access_field_null() {
        let context = &mut Context::new();
        let field = Field::new(
            Value::Null,
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.eval(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor 'null' must be of type String".to_string()
        );
    }
}
