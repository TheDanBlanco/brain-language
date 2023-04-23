use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context, output::Output, token::BrainToken, value::Value, Parse, Resolve,
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
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Function)?;

        let identifier = stream.expect(BrainToken::Identifier)?.clone();

        stream.expect(BrainToken::LeftParen)?;

        let mut arguments = vec![];

        while !stream.check(BrainToken::RightParen) {
            let next = stream.expect(BrainToken::Identifier)?.clone();

            arguments.push(next.data);

            stream.skip_if(BrainToken::Comma);
        }

        stream.expect(BrainToken::RightParen)?;

        let block = Statement::parse(stream)?;

        let definition = Self::new(identifier.data, arguments, block);

        stream.skip_if(BrainToken::Semicolon);

        Ok(definition)
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::grammar::{
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
            Token::new(0..2, BrainToken::Function, "fn".to_string()),
            Token::new(3..4, BrainToken::Identifier, "x".to_string()),
            Token::new(5..6, BrainToken::LeftParen, "(".to_string()),
            Token::new(7..8, BrainToken::RightParen, ")".to_string()),
            Token::new(9..10, BrainToken::LeftBrace, "{".to_string()),
            Token::new(11..12, BrainToken::RightBrace, "}".to_string()),
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
            Token::new(0..2, BrainToken::Function, "fn".to_string()),
            Token::new(3..4, BrainToken::Identifier, "adder".to_string()),
            Token::new(5..6, BrainToken::LeftParen, "(".to_string()),
            Token::new(7..8, BrainToken::Identifier, "x".to_string()),
            Token::new(9..10, BrainToken::Comma, ",".to_string()),
            Token::new(11..12, BrainToken::Identifier, "y".to_string()),
            Token::new(13..14, BrainToken::RightParen, ")".to_string()),
            Token::new(15..16, BrainToken::LeftBrace, "{".to_string()),
            Token::new(17..18, BrainToken::RightBrace, "}".to_string()),
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
    fn parse_function_definition_with_eof() {
        let tokens = vec![Token::new(0..2, BrainToken::Function, "fn".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionDefinition::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected Identifier, but found End of File".to_string()
        );
    }

    #[test]
    fn parse_function_definition_with_eof_args() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Function, "fn".to_string()),
            Token::new(3..4, BrainToken::Identifier, "adder".to_string()),
            Token::new(5..6, BrainToken::LeftParen, "(".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionDefinition::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected Identifier, but found End of File".to_string()
        );
    }
}
