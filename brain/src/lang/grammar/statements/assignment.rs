use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{
    context::Context, expressions::Expression, output::Output, Evaluate, Parse, Resolve,
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

impl Resolve for Assignment {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let value = self.value.evaluate(context)?;

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

impl Parse for Assignment {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::Let)?;

        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected identifier, found End of File".to_string(),
            ));
        }

        let identifer = next.unwrap().token.get_identifier()?;

        stream.expect(TokenKind::Assign)?;

        let expression = Expression::parse(stream)?;

        stream.skip_if(TokenKind::Semicolon);

        let assignment = Self::new(identifer, expression);

        Ok(assignment)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use brain_token::token::Token;

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

    #[test]
    fn parse_assignment() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::String("hello".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new(
                "x".to_string(),
                Expression::new_literal(Value::String("hello".to_string())),
            )
        );
    }

    #[test]
    fn parse_assignment_number() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new("x".to_string(), Expression::new_literal(Value::Number(0)),)
        );
    }

    #[test]
    fn parse_assignment_null() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Null),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new("x".to_string(), Expression::new_literal(Value::Null),)
        );
    }

    #[test]
    fn parse_assignment_bool() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::True),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new(
                "x".to_string(),
                Expression::new_literal(Value::Boolean(true)),
            )
        );
    }

    #[test]
    fn parse_assignment_collection() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::LeftBracket),
            Token::new(0, 0, TokenKind::RightBracket),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new("x".to_string(), Expression::new_collection(vec![]),)
        );
    }

    #[test]
    fn parse_assignment_map() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".into())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::LeftBrace),
            Token::new(0, 0, TokenKind::RightBrace),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new("x".to_string(), Expression::new_map(vec![]),)
        );
    }

    #[test]
    fn parse_assignment_eof() {
        let tokens = vec![Token::new(0, 0, TokenKind::Let)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected identifier, found End of File".to_string()
        );
    }
}
