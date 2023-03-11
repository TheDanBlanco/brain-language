use crate::lang::{
    grammar::{context::Context, output::Output, Parse, Resolve},
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue;

impl Resolve for Continue {
    fn resolve(&self, _context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Continue)
    }
}

impl Parse for Continue {
    fn parse(stream: &mut TokenStream) -> Result<Continue, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::Continue)?;
        stream.skip_if(TokenKind::Semicolon);

        Ok(Self)
    }
}

#[cfg(test)]
mod test {
    use crate::lang::tokens::token::Token;

    use super::*;

    #[test]
    fn resolve_continue() {
        let context = &mut Context::new();
        let r#break = Continue {};

        let result = r#break.resolve(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Continue,)
    }

    #[test]
    fn parse_continue() {
        let tokens = vec![Token::new(0, 0, TokenKind::Continue)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Continue::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Continue);
    }
}
