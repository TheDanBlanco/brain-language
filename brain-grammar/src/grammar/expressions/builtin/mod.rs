use crate::grammar::{context::Context, output::Output};

use self::print::{Print, PRINT};

use super::Expression;

mod print;

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
            _ => Err(format!("Unknown builtin: {}", name).into()),
        }?;

        Ok(out)
    }

    pub fn matches(name: String) -> bool {
        match name.as_str() {
            PRINT => true,
            _ => false,
        }
    }
}
