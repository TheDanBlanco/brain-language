use crate::grammar::{
    context::Context,
    expressions::Expression,
    output::Output,
    value::{complex::ComplexValue, literal::LiteralValue, Value},
    Evaluate,
};

use brain_error::{Error, ErrorKind};

pub const LEN: &str = "len";

pub struct Len;

impl Len {
    pub fn resolve(
        &self,
        context: &mut Context,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        let mut out = 0;
        if arguments.len() != 1 {
            return Err(Error::new(
                ErrorKind::InvalidArguments,
                format!("Invalid type to append to map. Expected map"),
            ));
        }

        let argument = arguments.first().unwrap();
        let value = argument.evaluate(context)?;

        match value {
            Value::Complex(ComplexValue::Collection(c)) => {
                out += c.value.len() as i64;
            }
            Value::Literal(LiteralValue::String(s)) => {
                out += s.chars().count() as i64;
            }
            Value::Complex(ComplexValue::Map(m)) => {
                out += m.value.len() as i64;
            }
            _ => panic!("invalid type for `len`"),
        }

        Ok(Output::Value(Value::Literal(LiteralValue::Number(out))))
    }
}
