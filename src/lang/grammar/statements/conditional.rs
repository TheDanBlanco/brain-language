use crate::lang::grammar::{
    context::Context,
    error::{Error, ErrorKind},
    expressions::{Evaluatable, Expression},
    output::Output,
    value::Value,
    Resolveable,
};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Conditional {
    condition: Expression,
    consequence: Box<Statement>,
    alternative: Box<Option<Statement>>,
}

impl Conditional {
    pub fn new(
        condition: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Self {
        Conditional {
            condition: condition,
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }
}

impl Resolveable for Conditional {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let condition = self.condition.eval(context)?;

        if !matches!(
            condition,
            Value::Null | Value::Boolean(_) | Value::Number(_) | Value::String(_)
        ) {
            return Err(Error::new(
                ErrorKind::InvalidType,
                format!("'{condition}' cannot be used as a boolean."),
            ));
        }

        let truthy = match condition {
            Value::Boolean(true) => true,
            Value::String(string) if string.len() != 0 => true,
            Value::Number(number) if number != 0 => true,
            _ => false,
        };

        if truthy {
            return self.consequence.resolve(context);
        }

        if let Some(alternative) = *self.alternative.clone() {
            return alternative.resolve(context);
        }

        Ok(Output::None)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::{
        expressions::operator::{mathematical::Mathematical, Operator},
        Node,
    };

    use super::*;

    #[test]
    fn new_conditional() {
        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);

        assert_eq!(
            conditional,
            Conditional {
                condition,
                consequence: Box::new(consequence),
                alternative: Box::new(None),
            }
        )
    }

    #[test]
    fn new_conditional_with_alternate() {
        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Subtract),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(
            condition.clone(),
            consequence.clone(),
            Some(alternate.clone()),
        );

        assert_eq!(
            conditional,
            Conditional {
                condition,
                consequence: Box::new(consequence),
                alternative: Box::new(Some(alternate)),
            }
        )
    }

    #[test]
    fn resolve_conditional_null() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Null);
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Subtract),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_true_no_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_false_no_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(false));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(0)
        )
    }

    #[test]
    fn resolve_conditional_true_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Subtract),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_false_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(false));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Subtract),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_string_not_empty() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::String("test".to_string()));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_string_empty() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::String("".to_string()));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Subtract),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_number_not_zero() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Number(1));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_number_zero() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Number(0));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::Mathematical(Mathematical::Add),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(0)
        )
    }

    #[test]
    fn resolve_conditional_invalid_conditional_collection() {
        let context = &mut Context::new();
        let condition = Expression::new_collection(vec![]);
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '[]' cannot be used as a boolean."
        );
    }

    #[test]
    fn resolve_conditional_invalid_conditional_map() {
        let context = &mut Context::new();
        let condition = Expression::new_map(vec![]);
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '{}' cannot be used as a boolean."
        );
    }

    #[test]
    fn resolve_conditional_invalid_conditional_function() {
        let context = &mut Context::new();
        let condition =
            Expression::new_literal(Value::Function(vec![], Box::new(Statement::new_break())));
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '[function]' cannot be used as a boolean."
        );
    }
}
