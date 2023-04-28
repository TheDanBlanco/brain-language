use crate::grammar::{context::Context, expressions::Expression, output::Output, Evaluate, value::{Value, complex::ComplexValue, literal::LiteralValue}};

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
            panic!("incorrect number of arguments to len(). expected: 1");
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
