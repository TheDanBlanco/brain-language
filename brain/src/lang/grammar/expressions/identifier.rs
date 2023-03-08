use crate::lang::{
    grammar::{
        context::Context,
        error::{Error, ErrorKind},
        value::Value,
        Evaluate, Parse,
    },
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

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

        if let TokenKind::Identifier(string) = &next.unwrap().token {
            return Ok(Self::new(string.to_string()));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedExpression,
            format!("Expected identifier, found {next:#?}"),
        ));
    }
}

// tests
#[cfg(test)]
mod tests {
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
}
