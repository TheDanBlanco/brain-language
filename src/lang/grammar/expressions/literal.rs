use crate::lang::grammar::{context::Context, value::Value};

use super::Evaluatable;

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
    fn eval(&self, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        Ok(self.value.clone())
    }
}

// tests
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::grammar::{statements::Statement, value::Value};

    #[test]
    fn create_new_number_literal() {
        let literal = Literal::new(Value::Number(1));
        assert_eq!(literal.value, Value::Number(1));
    }

    #[test]
    fn eval_number_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Number(1));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn create_new_string_literal() {
        let literal = Literal::new(Value::String("hello".to_string()));
        assert_eq!(literal.value, Value::String("hello".to_string()));
    }

    #[test]
    fn eval_string_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::String("hello".to_string()));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::String("hello".to_string()));
    }

    #[test]
    fn create_new_boolean_literal() {
        let literal = Literal::new(Value::Boolean(true));
        assert_eq!(literal.value, Value::Boolean(true));
    }

    #[test]
    fn eval_boolean_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Boolean(true));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn create_new_map_literal() {
        let literal = Literal::new(Value::Map(std::collections::BTreeMap::new()));
        assert_eq!(literal.value, Value::Map(std::collections::BTreeMap::new()));
    }

    #[test]
    fn eval_map_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Map(std::collections::BTreeMap::new()));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::Map(std::collections::BTreeMap::new())
        );
    }

    #[test]
    fn create_new_collection_literal() {
        let literal = Literal::new(Value::Collection(vec![]));
        assert_eq!(literal.value, Value::Collection(vec![]));
    }

    #[test]
    fn eval_collection_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Collection(vec![]));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Collection(vec![]));
    }

    #[test]
    fn create_new_function_literal() {
        let literal = Literal::new(Value::Function(vec![], Box::new(Statement::new_break())));
        assert_eq!(
            literal.value,
            Value::Function(vec![], Box::new(Statement::new_break()))
        );
    }

    #[test]
    fn eval_function_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Function(vec![], Box::new(Statement::new_break())));
        let result = literal.eval(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::Function(vec![], Box::new(Statement::new_break()))
        );
    }
}
