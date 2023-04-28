use crate::grammar::{context::Context, expressions::Expression, output::Output, Evaluate, value::{Value, complex::ComplexValue, literal::LiteralValue}};

pub const APPEND: &str = "append";

pub struct Append;

impl Append {
    pub fn resolve(
        &self,
        context: &mut Context,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        if arguments.len() < 2 {
            panic!("incorrect number of arguments to append(). expected at least: 2");
        }

        let input = arguments.first().unwrap().evaluate(context)?;

        match input {
            Value::Complex(ComplexValue::Collection(c)) => {
                let mut out = c.value.clone();
                for argument in arguments[1..].iter() {
                    let value = argument.evaluate(context)?;
                    out.push(value)
                }            

                return Ok(Output::Value(Value::new_collection(out)))
            }
           //Value::Map(m) => {
           //    let mut out = m.clone();
           //    for argument in arguments[1..].iter() {
           //        let value = argument.evaluate(context)?;
           //        match value {
           //            Value::Map(a) => {
           //                if out.contains_key(a.first_key())
           //                a 
           //            }
           //            _ => panic!("invalid type to append to map expected map")
           //        }
           //    }            

           //    return Ok(Output::Value(Value::Collection(out)))
           //}
            _ => panic!("invalid type for `append`"),
        }
    }
}
