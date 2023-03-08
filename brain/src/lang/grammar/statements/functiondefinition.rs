use crate::lang::{
    grammar::{
        context::Context,
        error::{Error, ErrorKind},
        output::Output,
        value::Value,
        Parse, Resolve,
    },
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

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

impl Resolve for FunctionDefinition {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        context.symbols.insert(
            self.identifier.clone(),
            Value::new_function(self.arguments.clone(), *self.block.clone()),
        );

        Ok(Output::None)
    }
}

impl Parse for FunctionDefinition {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::Function)?;

        let next = stream.next();
        let identifier = next.unwrap().token.get_identifier()?;

        stream.expect(TokenKind::LeftParen)?;

        let mut arguments = vec![];

        while !stream.check(TokenKind::RightParen) {
            let next = stream.next();

            if next.is_none() {
                return Err(Error::new(
                    ErrorKind::UnexpectedEndOfFile,
                    "Expected function argument, found End of File".to_string(),
                ));
            }

            let argument = next.unwrap().token.get_identifier()?;
            arguments.push(argument.to_string());
            stream.skip_if(TokenKind::Comma);
        }

        stream.expect(TokenKind::RightParen)?;

        let block = Statement::parse(stream)?;

        let definition = Self::new(identifier, arguments, block);

        stream.skip_if(TokenKind::Semicolon);

        Ok(definition)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::{
        context::Context,
        expressions::{operator::Operator, Expression},
        statements::{r#return::Return, Statement},
        value::Value,
        Node, Resolve,
    };

    use super::FunctionDefinition;

    #[test]
    fn new_function_definition() {
        let identifier = "adder".to_string();
        let arguments = vec!["a".to_string(), "b".to_string()];
        let block = Statement::new_block(vec![Node::from_statement(Statement::Return(
            Return::new(Expression::new_binary(
                Expression::new_identifier("a".to_string()),
                Operator::new_addition(),
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
                Operator::new_addition(),
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
