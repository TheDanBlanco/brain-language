use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context, expressions::Expression, output::Output, token::BrainToken, Evaluate, Parse,
    Resolve,
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
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Let)?;

        let identifier = stream.expect(BrainToken::Identifier)?.clone();

        stream.expect(BrainToken::Assign)?;

        let expression = Expression::parse(stream)?;

        stream.skip_if(BrainToken::Semicolon);

        let assignment = Self::new(identifier.data, expression);

        Ok(assignment)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use brain_token::token::Token;

    use crate::grammar::{statements::Statement, value::Value};

    use super::*;

    #[test]
    fn new_assignment() {
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_number(1));

        let assignment = Assignment::new(target.clone(), value.clone());
        assert_eq!(assignment.target, target);
        assert_eq!(assignment.value, value);
    }

    #[test]
    fn resolve_assignemnt_number() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_number(1));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(context.symbols.get("foo").unwrap(), &Value::new_number(1));
    }

    #[test]
    fn resolve_assignemnt_string() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_string("a".to_string()));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::new_string("a".to_string())
        );
    }

    #[test]
    fn resolve_assignemnt_null() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_null());

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(context.symbols.get("foo").unwrap(), &Value::new_null());
    }

    #[test]
    fn resolve_assignemnt_collection() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_collection(vec![]));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::new_collection(vec![])
        );
    }

    #[test]
    fn resolve_assignemnt_map() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_map(BTreeMap::new()));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::new_map(BTreeMap::new())
        );
    }

    #[test]
    fn resolve_assignment_function() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_function(vec![], Statement::new_break()));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::new_function(vec![], Statement::new_break())
        );
    }

    #[test]
    fn resolve_assignment_boolean() {
        let context = &mut Context::new();
        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_boolean(true));

        let assignment = Assignment::new(target, value);
        let result = assignment.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("foo").unwrap(),
            &Value::new_boolean(true)
        );
    }

    #[test]
    fn resolve_assignment_identifier_already_exists() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::new_null());

        let target = "foo".to_string();
        let value = Expression::new_literal(Value::new_boolean(true));

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
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..9, BrainToken::String, r#""hello""#.into()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new(
                "x".to_string(),
                Expression::new_literal(Value::new_string("hello".to_string())),
            )
        );
    }

    #[test]
    fn parse_assignment_number() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..9, BrainToken::Number, "0".into()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new(
                "x".to_string(),
                Expression::new_literal(Value::new_number(0)),
            )
        );
    }

    #[test]
    fn parse_assignment_null() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..11, BrainToken::Null, "null".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new("x".to_string(), Expression::new_literal(Value::new_null()),)
        );
    }

    #[test]
    fn parse_assignment_bool() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..11, BrainToken::True, "true".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Assignment::new(
                "x".to_string(),
                Expression::new_literal(Value::new_boolean(true)),
            )
        );
    }

    #[test]
    fn parse_assignment_collection() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..9, BrainToken::LeftBracket, "[".to_string()),
            Token::new(10..11, BrainToken::RightBracket, "]".to_string()),
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
            Token::new(0..3, BrainToken::Let, "let".to_string()),
            Token::new(4..5, BrainToken::Identifier, "x".into()),
            Token::new(6..7, BrainToken::Assign, "=".to_string()),
            Token::new(8..9, BrainToken::LeftBrace, "{".to_string()),
            Token::new(10..11, BrainToken::RightBrace, "}".to_string()),
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
        let tokens = vec![Token::new(0..3, BrainToken::Let, "let".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Assignment::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected Identifier, but found End of File".to_string()
        );
    }
}
