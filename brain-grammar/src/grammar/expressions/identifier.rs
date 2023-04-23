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
        let next = stream.expect(BrainToken::Identifier)?;

        return Ok(Identifier::new(next.data.clone()));
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
            .insert("hello".to_string(), Value::new_number(1));
        let identifier = Identifier::new("hello".to_string());
        let result = identifier.evaluate(&mut context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
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
        let tokens = vec![Token::new(0..1, BrainToken::Identifier, "a".to_string())];

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
            "[UnexpectedEndOfFile]: Expected Identifier, but found End of File".to_string()
        );
    }

    #[test]
    fn parse_identifier_not_identifier() {
        let tokens = vec![Token::new(0..3, BrainToken::Null, "null".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Identifier::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected Identifier, found Null (0 - 3)".to_string()
        );
    }
}
