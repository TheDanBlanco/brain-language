use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{context::Context, output::Output, value::Value, Parse, Resolve};

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
    use brain_token::token::Token;

    use crate::lang::grammar::{
        expressions::{operator::Operator, Expression},
        statements::r#return::Return,
        Node,
    };

    use super::*;

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

    #[test]
    fn parse_function_definition() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Function),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::RightParen),
            Token::new(0, 0, TokenKind::LeftBrace),
            Token::new(0, 0, TokenKind::RightBrace),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionDefinition::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionDefinition::new("x".to_string(), vec![], Statement::new_block(vec![]))
        );
    }

    #[test]
    fn parse_function_definition_with_args() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Function),
            Token::new(0, 0, TokenKind::Identifier("adder".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Comma),
            Token::new(0, 0, TokenKind::Identifier("y".to_string())),
            Token::new(0, 0, TokenKind::RightParen),
            Token::new(0, 0, TokenKind::LeftBrace),
            Token::new(0, 0, TokenKind::RightBrace),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionDefinition::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionDefinition::new(
                "adder".to_string(),
                vec!["x".to_string(), "y".to_string()],
                Statement::new_block(vec![])
            )
        );
    }

    #[test]
    fn parse_function_definition_with_eof_args() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Function),
            Token::new(0, 0, TokenKind::Identifier("adder".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionDefinition::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected function argument, found End of File".to_string()
        );
    }
}
