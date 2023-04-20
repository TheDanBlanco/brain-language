use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    name: String,
    variant: String,
}

impl Enum {
    pub fn new(name: String, variant: String) -> Self {
        Self { name, variant }
    }
}

impl Parse for Enum {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let name = stream.expect(BrainToken::Identifier)?.clone();

        stream.expect(BrainToken::Separator)?;

        let variant = stream.expect(BrainToken::Identifier)?.clone();

        Ok(Self::new(name.data, variant.data))
    }
}

impl Evaluate for Enum {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let definition = context.symbols.get(&self.name);

        if definition.is_none() {
            return Err(Error::new(
                ErrorKind::UnknownIdentifier,
                format!("{}", &self.name),
            ));
        }

        if let Value::EnumDefinition(_, variants) = definition.unwrap() {
            if !variants.contains(&self.variant) {
                return Err(Error::new(
                    ErrorKind::UnknownEnumVariant,
                    format!(
                        "{} is not a valid variant for enum '{}'",
                        &self.variant, &self.name
                    ),
                ));
            }

            return Ok(Value::EnumVariant(self.name.clone(), self.variant.clone()));
        }

        return Err(Error::new(
            ErrorKind::UnknownEnum,
            format!("{} is not an enum", &self.name),
        ));
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn new_enum() {
        let name = "test".to_string();
        let variant = "one".to_string();

        let r#enum = Enum::new(name.clone(), variant.clone());

        assert_eq!(r#enum, Enum { variant, name })
    }

    #[test]
    fn parse_enum() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, "test".to_string()),
            Token::new(0..1, BrainToken::Separator, "::".to_string()),
            Token::new(0..1, BrainToken::Identifier, "one".to_string()),
            Token::new(0..1, BrainToken::Semicolon, ";".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Enum::parse(stream);

        assert_eq!(
            result.unwrap(),
            Enum {
                name: "test".to_string(),
                variant: "one".to_string()
            }
        )
    }

    #[test]
    fn evaluate_enum() {
        let context = &mut Context::new();
        context.symbols.insert(
            "test".to_string(),
            Value::EnumDefinition("test".to_string(), vec!["a".to_string(), "b".to_string()]),
        );

        let r#enum = Enum::new("test".to_string(), "a".to_string());

        let result = r#enum.evaluate(context);

        assert_eq!(
            result.unwrap(),
            Value::EnumVariant("test".to_string(), "a".to_string())
        )
    }

    #[test]
    fn evaluate_enum_not_defined() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::EnumDefinition("foo".to_string(), vec!["a".to_string(), "b".to_string()]),
        );

        let r#enum = Enum::new("test".to_string(), "a".to_string());

        let result = r#enum.evaluate(context);

        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnknownIdentifier]: test",
        )
    }

    #[test]
    fn evaluate_enum_not_an_enum() {
        let context = &mut Context::new();
        context
            .symbols
            .insert("test".to_string(), Value::String("foo".to_string()));

        let r#enum = Enum::new("test".to_string(), "a".to_string());

        let result = r#enum.evaluate(context);

        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnknownEnum]: test is not an enum",
        )
    }

    #[test]
    fn evaluate_enum_variant_not_defined() {
        let context = &mut Context::new();
        context.symbols.insert(
            "test".to_string(),
            Value::EnumDefinition("foo".to_string(), vec!["a".to_string(), "b".to_string()]),
        );

        let r#enum = Enum::new("test".to_string(), "c".to_string());

        let result = r#enum.evaluate(context);

        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnknownEnumVariant]: c is not a valid variant for enum 'test'",
        )
    }
}
