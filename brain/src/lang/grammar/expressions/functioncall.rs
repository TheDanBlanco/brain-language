use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{
    context::Context, output::Output, value::Value, Evaluate, Match, Parse, Resolve,
};

use super::{binary::Binary, identifier::Identifier, operator::Operator, Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall {
    pub identifier: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl FunctionCall {
    pub fn new(identifier: Expression, arguments: Vec<Expression>) -> Self {
        FunctionCall {
            identifier: Box::new(identifier),
            arguments,
        }
    }

    pub fn parse(
        stream: &mut TokenStream,
        initial: Option<Expression>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let identifier = match initial {
            Some(expression) => expression,
            None => Expression::Identifier(Identifier::parse(stream)?),
        };

        stream.expect(TokenKind::LeftParen)?;

        let mut arguments = vec![];

        while !stream.check(TokenKind::RightParen) {
            let mut expression = Expression::parse(stream)?;

            let next = stream.peek();

            if next.is_none() {
                return Err(
                    Error::new(
                        ErrorKind::UnexpectedEndOfFile,
                        "Expected binary expression, function call, or ending parenthesis, found End of File".to_string()
                    )
                );
            }

            let token = &next.unwrap().token;

            if Operator::matches(token) {
                let binary = Binary::parse(stream, Some(expression))?;
                expression = Expression::Binary(binary);
            }

            stream.skip_if(TokenKind::Comma);

            arguments.push(expression);
        }

        stream.expect(TokenKind::RightParen)?;

        stream.skip_if(TokenKind::Semicolon);

        Ok(Self::new(identifier, arguments))
    }
}

impl Evaluate for FunctionCall {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let identifier = self.identifier.evaluate(context)?;

        let function = match identifier.clone() {
            Value::Function(_, _) => identifier,
            Value::String(fn_identifier) => match context.symbols.get(&fn_identifier) {
                Some(Value::Function(function, args)) => {
                    Value::Function(function.clone(), args.clone())
                }
                Some(_) => {
                    return Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("'{}' is not a function", fn_identifier),
                    ))
                }
                None => {
                    return Err(Error::new(
                        ErrorKind::UnknownIdentifier,
                        format!("'{fn_identifier}'"),
                    ))
                }
            },
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidType,
                    format!("'{identifier}' is not a neither a function nor an identifier"),
                ));
            }
        };

        if let Value::Function(function_arguments, block) = function {
            let mut zipped_arguments = function_arguments.iter().zip(self.arguments.clone());
            let local_context = &mut context.clone_and_merge_symbols(&mut zipped_arguments)?;

            if let Output::Value(value) = block.resolve(local_context)? {
                return Ok(value);
            }
        }

        Ok(Value::Null)
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::statements::Statement;

    use super::*;

    #[test]
    fn create_new_function_call() {
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );
        assert_eq!(
            function_call.identifier,
            Box::new(Expression::new_literal(Value::String("foo".to_string())))
        );
        assert_eq!(
            function_call.arguments,
            vec![Expression::new_literal(Value::Number(1))]
        );
    }

    #[test]
    fn eval_function_call_identifier_is_string() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::new_function(vec![], Statement::new_break()),
        );

        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![],
        );

        let result = function_call.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Null);
    }

    #[test]
    fn eval_function_call_identifier_is_function() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::new_function(
                vec![],
                Statement::new_return(Expression::new_literal(Value::Number(0))),
            )),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(0),);
    }

    #[test]
    fn eval_function_call_unknown_identifier() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.evaluate(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnknownIdentifier]: 'foo'"
        )
    }

    #[test]
    fn eval_function_call_invalid_type() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::Number(1)),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.evaluate(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '1' is not a neither a function nor an identifier"
        )
    }

    #[test]
    fn eval_function_type_in_symbol_table_not_a_function() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.evaluate(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: 'foo' is not a function"
        )
    }

    #[test]
    fn parse_function_call() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("foo".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::RightParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, None);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionCall::new(Expression::new_identifier("foo".to_string()), vec![])
        );
    }

    #[test]
    fn parse_function_call_with_initial() {
        let initial = Expression::new_identifier("foo".to_string());

        let tokens = vec![
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::RightParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, Some(initial));

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionCall::new(Expression::new_identifier("foo".to_string()), vec![])
        );
    }

    #[test]
    fn parse_function_call_with_args() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("foo".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::Identifier("a".to_string())),
            Token::new(0, 0, TokenKind::Comma),
            Token::new(0, 0, TokenKind::Identifier("b".to_string())),
            Token::new(0, 0, TokenKind::RightParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, None);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionCall::new(
                Expression::new_identifier("foo".to_string()),
                vec![
                    Expression::new_identifier("a".to_string()),
                    Expression::new_identifier("b".to_string())
                ]
            )
        );
    }

    #[test]
    fn parse_function_call_with_function_arg() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("foo".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::Identifier("a".to_string())),
            Token::new(0, 0, TokenKind::Comma),
            Token::new(0, 0, TokenKind::Identifier("b".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::RightParen),
            Token::new(0, 0, TokenKind::RightParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, None);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionCall::new(
                Expression::new_identifier("foo".to_string()),
                vec![
                    Expression::new_identifier("a".to_string()),
                    Expression::new_function_call(
                        Expression::new_identifier("b".to_string()),
                        vec![],
                    )
                ]
            )
        );
    }

    #[test]
    fn parse_function_call_with_binary_arg() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("foo".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::Identifier("a".to_string())),
            Token::new(0, 0, TokenKind::Comma),
            Token::new(0, 0, TokenKind::Number(0)),
            Token::new(0, 0, TokenKind::Add),
            Token::new(0, 0, TokenKind::Number(1)),
            Token::new(0, 0, TokenKind::RightParen),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, None);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            FunctionCall::new(
                Expression::new_identifier("foo".to_string()),
                vec![
                    Expression::new_identifier("a".to_string()),
                    Expression::new_binary(
                        Expression::new_literal(Value::Number(0)),
                        Operator::new_addition(),
                        Expression::new_literal(Value::Number(1)),
                    )
                ]
            )
        );
    }

    #[test]
    fn parse_function_call_eof() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("foo".to_string())),
            Token::new(0, 0, TokenKind::LeftParen),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = FunctionCall::parse(stream, None);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected binary expression, function call, or ending parenthesis, found End of File".to_string()
        );
    }
}
