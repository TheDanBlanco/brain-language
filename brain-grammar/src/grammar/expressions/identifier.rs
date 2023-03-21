use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Identifier { name }
    }
}

impl Evaluate for Identifier {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        if let Some(value) = context.symbols.get(&self.name) {
            return Ok(value.clone());
        }

        return Err(Error::new(
            ErrorKind::UnknownIdentifier,
            format!("'{}'", self.name),
        ));
    }
}

impl Parse for Identifier {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                format!("Expected identifier, found End of File"),
            ));
        }

        let token = next.unwrap();

        if let BrainToken::Identifier = &token.token {
            return Ok(Identifier::new(token.data.clone().unwrap()));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedExpression,
            format!(
                "Expected identifier, found {} ({} - {})",
                token.token, token.span.start, token.span.end
            ),
        ));
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;
    use crate::grammar::value::Value;

    #[test]
    fn create_new_identifier() {
        let identifier = Identifier::new("hello".to_string());
        assert_eq!(identifier.name, "hello");
    }

    #[test]
    fn eval_identifier() {
        let mut context = Context::new();
        context
            .symbols
            .insert("hello".to_string(), Value::Number(1));
        let identifier = Identifier::new("hello".to_string());
        let result = identifier.evaluate(&mut context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn eval_identifier_not_found() {
        let mut context = Context::new();
        let identifier = Identifier::new("hello".to_string());
        let result = identifier.evaluate(&mut context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[UnknownIdentifier]: 'hello'",
        );
    }

    #[test]
    fn parse_identifier() {
        let tokens = vec![Token::new(
            0..1,
            BrainToken::Identifier,
            Some("a".to_string()),
        )];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Identifier::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Identifier::new("a".to_string()));
    }

    #[test]
    fn parse_identifier_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Identifier::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected identifier, found End of File".to_string()
        );
    }

    #[test]
    fn parse_identifier_not_identifier() {
        let tokens = vec![Token::new(0..3, BrainToken::Null, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Identifier::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedExpression]: Expected identifier, found Token::Null (0 - 3)".to_string()
        );
    }
}
