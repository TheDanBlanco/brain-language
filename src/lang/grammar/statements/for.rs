use crate::lang::grammar::{
    context::Context,
    expressions::{Evaluatable, Expression},
    output::Output,
    value::Value,
    Resolveable,
};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct For {
    identifier: String,
    iterable: Expression,
    block: Box<Statement>,
}

impl For {
    pub fn new(identifier: String, iterable: Expression, block: Statement) -> Self {
        For {
            identifier,
            iterable,
            block: Box::new(block),
        }
    }
}

impl Resolveable for For {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let iterable = self.iterable.eval(context)?;

        if let Value::Collection(collection) = iterable {
            for value in collection {
                context.symbols.insert(self.identifier.clone(), value);
                let out = self.block.resolve(context)?;

                if matches!(out, Output::Break) {
                    return Ok(out);
                }
            }
        }

        Ok(Output::None)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::{expressions::operator::Operator, Node};

    use super::*;

    #[test]
    fn new_for() {
        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);
        let block =
            Statement::new_block(vec![Node::from_expression(Expression::new_function_call(
                Expression::new_identifier("print".to_string()),
                vec![Expression::new_identifier("item".to_string())],
            ))]);

        let r#for = For::new(identifier.clone(), iterable.clone(), block.clone());

        assert_eq!(
            r#for,
            For {
                identifier: identifier,
                iterable: iterable,
                block: Box::new(block),
            }
        )
    }

    #[test]
    fn resolve_for() {
        let context = &mut Context::new();

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);
        let block = Statement::new_block(vec![]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
    }

    #[test]
    fn resolve_for_loop_count() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);

        let block = Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
            "x".to_string(),
            Expression::new_binary(
                Expression::new_identifier("x".to_string()),
                Operator::new_addition(),
                Expression::new_identifier("item".to_string()),
            ),
        ))]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);

        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(6),
        )
    }

    #[test]
    fn resolve_early_break() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);

        let block = Statement::new_block(vec![
            Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_identifier("item".to_string()),
                ),
            )),
            Node::from_statement(Statement::new_break()),
        ]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Break);

        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1),
        )
    }
}
