use crate::lang::grammar::{
    context::Context,
    error::{Error, ErrorKind},
    expressions::{Evaluatable, Expression},
    output::Output,
    Resolveable,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Assignment {
    target: String,
    value: Expression,
}

impl Assignment {
    pub fn new(target: String, value: Expression) -> Self {
        Assignment {
            target: target,
            value: value,
        }
    }
}

impl Resolveable for Assignment {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let value = self.value.eval(context)?;

        if context.symbols.get(&self.target).is_some() {
            return Err(Error::new(
                ErrorKind::IdentifierAlreadyExists,
                format!("'{}'", self.target),
            ));
        }

        context.symbols.insert(self.target.clone(), value);

        return Ok(Output::None);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::lang::grammar::{statements::Statement, value::Value};

    use super::*;

    #[test]
    fn new_assignment() {
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Number(1));

        let assignment = Assignment::new(target.clone(), value.clone());
        assert_eq!(assignment.target, target);
        assert_eq!(assignment.value, value);
    }

    #[test]
    fn resolve_assignemnt_number() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Number(1));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(context.symbols.get("foo").unwrap(), &Value::Number(1));
    }

    #[test]
    fn resolve_assignemnt_string() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::String("a".to_string()));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::String("a".to_string())
        );
    }

    #[test]
    fn resolve_assignemnt_null() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Null);

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(context.symbols.get("foo").unwrap(), &Value::Null);
    }

    #[test]
    fn resolve_assignemnt_collection() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Collection(vec![]));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::Collection(vec![])
        );
    }

    #[test]
    fn resolve_assignemnt_map() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Map(BTreeMap::new()));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::Map(BTreeMap::new())
        );
    }

    #[test]
    fn resolve_assignment_function() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value =
            Expression::new_literal(Value::Function(vec![], Box::new(Statement::new_break())));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::Function(vec![], Box::new(Statement::new_break()))
        );
    }

    #[test]
    fn resolve_assignment_boolean() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Boolean(true));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(context.symbols.get("foo").unwrap(), &Value::Boolean(true));
    }

    #[test]
    fn resolve_assignment_identifier_already_exists() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Null);

        let target = "foo".to_string();
        let value = Expression::new_literal(Value::Boolean(true));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[IdentifierAlreadyExists]: 'foo'",
        )
    }
}
