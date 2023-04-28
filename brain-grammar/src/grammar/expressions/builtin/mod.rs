use crate::grammar::{context::Context, output::Output};

use self::len::{Len, LEN};
use self::print::{Print, PRINT};
use self::append::{Append, APPEND};

use super::Expression;

mod len;
mod print;
mod append;

pub enum Builtin {
    Print(Print),
}

impl Builtin {
    pub fn resolve(
        context: &mut Context,
        name: String,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        let out = match name.as_str() {
            PRINT => Print.resolve(context, arguments),
            LEN => Len.resolve(context, arguments),
            APPEND => Append.resolve(context, arguments),
            _ => Err(format!("Unknown builtin: {}", name).into()),
        }?;

        Ok(out)
    }

    pub fn matches(name: String) -> bool {
        match name.as_str() {
            PRINT => true,
            LEN => true,
            APPEND => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches() {
        assert!(Builtin::matches("print".to_string()));

        assert!(!Builtin::matches("DOES NOT MATCH ANYTHING".to_string()));
    }

    #[test]
    fn resolve_print() {
        let mut context = Context::new();

        let result = Builtin::resolve(&mut context, "print".to_string(), vec![]);

        assert!(result.is_ok())
    }

    #[test]
    fn resolve_not_found() {
        let mut context = Context::new();

        let result = Builtin::resolve(&mut context, "NOT FOUND".to_string(), vec![]);

        assert!(result.is_err())
    }
}
