use brain_token::stream::TokenStream;

use crate::lang::grammar::{context::Context, output::Output, token::BrainToken, Parse, Resolve};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break;

impl Resolve for Break {
    fn resolve(&self, _context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Break)
    }
}

impl Parse for Break {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Break)?;
        stream.skip_if(BrainToken::Semicolon);

        Ok(Self)
    }
}

#[cfg(test)]
mod test {

    use brain_token::token::Token;

    use super::*;

    #[test]
    fn resolve_break() {
        let context = &mut Context::new();
        let r#break = Break {};

        let result = r#break.resolve(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Break,)
    }

    #[test]
    fn parse_break() {
        let tokens = vec![Token::new(0..5, BrainToken::Break, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Break::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Break);
    }
}
