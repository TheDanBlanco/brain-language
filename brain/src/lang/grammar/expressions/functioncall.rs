use crate::lang::{
    grammar::{
        context::Context,
        error::{Error, ErrorKind},
        output::Output,
        value::Value,
        Evaluate, Match, Parse, Resolve,
    },
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

use super::{binary::Binary, operator::Operator, Expression};

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
        identifier: Expression,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::LeftParen)?;

        let mut arguments = vec![];

        while !stream.check(TokenKind::RightParen) {
            let mut expression = Expression::parse(stream)?;
            stream.skip_if(TokenKind::Comma);

            let next = stream.peek();

            if next.is_none() {
                return Err(
                    Error::new(
                        ErrorKind::UnexpectedEndOfFile,
                        "Expected binary expression, function call, or ending parenthesis, found End of File".to_string()
                    )
                );
            }

            if Operator::matches(&next.unwrap().token) {
                let binary = Binary::parse(stream, expression)?;
                expression = Expression::Binary(binary);
            }

            if stream.check(TokenKind::LeftParen) {
                let function_call = Self::parse(stream, expression)?;
                expression = Expression::FunctionCall(function_call);
            }

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
            Expression::new_literal(Value::new_function(
                vec![],
                Statement::new_return(Expression::new_literal(Value::Number(0))),
            )),
            vec![],
        );

        let result = function_call.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(0),);
    }

    #[test]
    fn eval_function_call_no_return_value() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::new_function(vec![], Statement::new_break()),
        );

        let function_call = FunctionCall::new(
            Expression::new_literal(Value::new_function(vec![], Statement::new_break())),
            vec![],
        );

        let result = function_call.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Null,);
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
}
