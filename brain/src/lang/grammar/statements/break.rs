use crate::lang::{
    grammar::{context::Context, output::Output, Parse, Resolve},
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break;

impl Resolve for Break {
    fn resolve(&self, _context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Break)
    }
}

impl Parse for Break {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::Break)?;
        stream.skip_if(TokenKind::Semicolon);

        Ok(Self)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn resolve_break() {
        let context = &mut Context::new();
        let r#break = Break {};

        let result = r#break.resolve(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Break,)
    }
}
