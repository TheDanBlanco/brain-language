use crate::lang::grammar::{context::Context, output::Output, Resolveable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break;

impl Resolveable for Break {
    fn resolve(&self, _context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Break)
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
