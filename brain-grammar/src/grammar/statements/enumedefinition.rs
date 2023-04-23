use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context, output::Output, token::BrainToken, value::Value, Parse, Resolve,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumDefinition {
    pub name: String,
    pub variants: Vec<String>,
}

impl EnumDefinition {
    pub fn new(name: String, variants: Vec<String>) -> Self {
        Self { name, variants }
    }
}

impl Parse for EnumDefinition {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Enum)?;

        let name = stream.expect(BrainToken::Identifier)?.clone();

        stream.expect(BrainToken::LeftBrace)?;

        let mut variants = vec![];

        while !stream.check(BrainToken::RightBrace) {
            let variant = stream.expect(BrainToken::Identifier)?.clone();

            stream.skip_if(BrainToken::Comma);

            variants.push(variant.data);
        }

        stream.expect(BrainToken::RightBrace)?;

        stream.skip_if(BrainToken::Semicolon);

        Ok(Self {
            name: name.data,
            variants,
        })
    }
}

impl Resolve for EnumDefinition {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        context.symbols.insert(
            self.name.clone(),
            Value::EnumDefinition(self.name.clone(), self.variants.clone()),
        );

        Ok(Output::None)
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn new_enum_definition() {
        let name = "Test".to_string();
        let variants = vec!["first".to_string(), "second".to_string()];

        let r#enum = EnumDefinition::new(name.clone(), variants.clone());

        assert_eq!(r#enum, EnumDefinition { name, variants })
    }

    #[test]
    fn resolve_enum_definition() {
        let context = &mut Context::new();

        let name = "test".to_string();
        let variants = vec!["a".to_string(), "b".to_string()];

        let definition = EnumDefinition::new(name.clone(), variants.clone());
        let result = definition.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            context.symbols.get("test").unwrap(),
            &Value::EnumDefinition(name, variants)
        )
    }

    #[test]
    fn parse_enum_definition() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Enum, "enum".to_string()),
            Token::new(0..3, BrainToken::Identifier, "test".to_string()),
            Token::new(4..5, BrainToken::LeftBrace, "{".to_string()),
            Token::new(6..7, BrainToken::Identifier, "a".to_string()),
            Token::new(7..8, BrainToken::Comma, ",".to_string()),
            Token::new(9..10, BrainToken::Identifier, "b".to_string()),
            Token::new(11..12, BrainToken::RightBrace, "}".to_string()),
            Token::new(12..13, BrainToken::Semicolon, ";".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = EnumDefinition::parse(stream);

        assert_eq!(
            result.unwrap(),
            EnumDefinition {
                name: "test".to_string(),
                variants: vec!["a".to_string(), "b".to_string()]
            }
        )
    }
}
