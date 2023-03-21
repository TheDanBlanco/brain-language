use crate::grammar::{context::Context, expressions::Expression, output::Output, Evaluate};

pub const PRINT: &str = "print";

pub struct Print;

impl Print {
    pub fn resolve(
        &self,
        context: &mut Context,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        let mut output = String::new();

        for argument in arguments {
            let value = argument.evaluate(context)?;

            output = format!("{} {}", output, value);
        }

        println!("{}", output);

        Ok(Output::None)
    }
}
