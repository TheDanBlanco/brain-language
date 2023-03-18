use brain_token::stream::TokenStream;

use crate::lang::grammar::{context::Context, output::Output, token::BrainToken, Parse, Resolve};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue;

impl Resolve for Continue {
    fn resolve(&self, _context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Continue)
    }
}

impl Parse for Continue {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Continue, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Continue)?;
        stream.skip_if(BrainToken::Semicolon);

        Ok(Self)
    }
}

#[cfg(test)]
mod test {

    use brain_token::token::Token;

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
        let tokens = vec![Token::new(0..7, BrainToken::Continue, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Continue::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Continue);
    }
}
