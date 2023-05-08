use crate::grammar::{context::Context, output::Output};

use self::append::{Append, APPEND};
use self::merge::{Merge, MERGE};
use self::remove::{Remove, REMOVE};
use self::len::{Len, LEN};
use self::print::{Print, PRINT};

use super::Expression;

mod append;
mod merge;
mod len;
mod print;
mod remove;

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
            MERGE => Merge.resolve(context, arguments),
            REMOVE => Remove.resolve(context, arguments),
            _ => Err(format!("Unknown builtin: {}", name).into()),
        }?;

        Ok(out)
    }

    pub fn matches(name: String) -> bool {
        match name.as_str() {
            PRINT => true,
            LEN => true,
            APPEND => true,
            MERGE => true,
            REMOVE => true,
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
