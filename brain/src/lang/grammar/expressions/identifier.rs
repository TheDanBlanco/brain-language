use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{context::Context, value::Value, Evaluate, Parse};

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
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                format!("Expected identifier, found End of File"),
            ));
        }

        let token = &next.unwrap().token;

        if let TokenKind::Identifier(string) = token {
            return Ok(Self::new(string.to_string()));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedExpression,
            format!("Expected identifier, found {token}"),
        ));
    }
}

// tests
#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;
    use crate::lang::grammar::value::Value;

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
        let tokens = vec![Token::new(0, 0, TokenKind::Identifier("a".to_string()))];

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
        let tokens = vec![Token::new(0, 0, TokenKind::Null)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Identifier::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedExpression]: Expected identifier, found Token::Null".to_string()
        );
    }
}
