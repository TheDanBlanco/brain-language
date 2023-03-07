use std::collections::BTreeMap;

use crate::lang::grammar::{
    context::Context,
    error::{Error, ErrorKind},
    value::Value,
    Evaluate,
};

use super::Expression;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Map {
    pub fn new(pairs: Vec<(Expression, Expression)>) -> Self {
        Map { pairs }
    }
}

impl Evaluate for Map {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut map = BTreeMap::new();

        for (key, value) in &self.pairs {
            let key = key.evaluate(context)?;

            match key {
                Value::Number(_) | Value::String(_) | Value::Boolean(_) => {}
                _ => return Err(Error::new(ErrorKind::InvalidMapKey, format!("{key}"))),
            }

            let value = value.evaluate(context)?;
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Map(map))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::grammar::{statements::Statement, value::Value};

    #[test]
    fn create_new_map() {
        let pairs = vec![(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        assert_eq!(map.pairs.len(), 1);
        assert_eq!(
            map.pairs[0].0,
            Expression::new_literal(Value::String("a".to_string()))
        );
    }

    #[test]
    fn eval_map_string_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::String("a".to_string()), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_number_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::Number(1), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_boolean_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::Boolean(true), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Boolean(true)),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_with_invalid_key_collection() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Collection(vec![])),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "[InvalidMapKey]: []",);
    }

    #[test]
    fn eval_map_with_invalid_key_function() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::new_function(vec![], Statement::new_break())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMapKey]: [function]",
        );
    }

    #[test]
    fn eval_map_with_invalid_key_null() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Null),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "[InvalidMapKey]: null",);
    }

    #[test]
    fn eval_map_with_invalid_key_map() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Map(BTreeMap::new())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "[InvalidMapKey]: {}",);
    }
}
