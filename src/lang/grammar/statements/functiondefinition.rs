use crate::lang::grammar::{output::Output, value::Value, Resolveable};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionDefinition {
    identifier: String,
    arguments: Vec<String>,
    block: Box<Statement>,
}

impl FunctionDefinition {
    pub fn new(identifier: String, arguments: Vec<String>, block: Statement) -> Self {
        FunctionDefinition {
            identifier,
            arguments,
            block: Box::new(block),
        }
    }
}

impl Resolveable for FunctionDefinition {
    fn resolve(
        &self,
        context: &mut crate::lang::grammar::context::Context,
    ) -> Result<crate::lang::grammar::output::Output, Box<dyn std::error::Error>> {
        context.symbols.insert(
            self.identifier.clone(),
            Value::Function(self.arguments.clone(), self.block.clone()),
        );

        Ok(Output::None)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::{
        context::Context,
        expressions::{
            operator::{mathematical::Mathematical, Operator},
            Expression,
        },
        statements::{r#return::Return, Statement},
        value::Value,
        Node, Resolveable,
    };

    use super::FunctionDefinition;

    #[test]
    fn new_function_definition() {
        let identifier = "adder".to_string();
        let arguments = vec!["a".to_string(), "b".to_string()];
        let block = Statement::new_block(vec![Node::from_statement(Statement::Return(
            Return::new(Expression::new_binary(
                Expression::new_identifier("a".to_string()),
                Operator::Mathematical(Mathematical::Add),
                Expression::new_identifier("b".to_string()),
            )),
        ))]);

        let definition =
            FunctionDefinition::new(identifier.clone(), arguments.clone(), block.clone());
        assert_eq!(
            definition,
            FunctionDefinition {
                identifier,
                arguments,
                block: Box::new(block)
            }
        )
    }

    #[test]
    fn resolve_function_definition() {
        let context = &mut Context::new();

        let identifier = "adder".to_string();
        let arguments = vec!["a".to_string(), "b".to_string()];
        let block = Statement::new_block(vec![Node::from_statement(Statement::Return(
            Return::new(Expression::new_binary(
                Expression::new_identifier("a".to_string()),
                Operator::Mathematical(Mathematical::Add),
                Expression::new_identifier("b".to_string()),
            )),
        ))]);

        let definition =
            FunctionDefinition::new(identifier.clone(), arguments.clone(), block.clone());
        let result = definition.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("adder").unwrap(),
            &Value::Function(arguments, Box::new(block))
        )
    }
}
