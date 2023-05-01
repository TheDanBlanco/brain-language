use crate::grammar::{
    context::Context,
    expressions::Expression,
    output::Output,
    value::{complex::ComplexValue, Value},
    Evaluate,
};

use brain_error::{Error, ErrorKind};

pub const APPEND: &str = "append";

pub struct Append;

impl Append {
    pub fn resolve(
        &self,
        context: &mut Context,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        if arguments.len() < 2 {
            return Err(Error::new(
                ErrorKind::InvalidArguments,
                format!("Cannot call append with less than 2 arguments"),
            ));
        }

        let input = arguments.first().unwrap().evaluate(context)?;

        match input {
            Value::Complex(ComplexValue::Collection(c)) => {
                let mut out = c.value.clone();
                for argument in arguments[1..].iter() {
                    let value = argument.evaluate(context)?;
                    out.push(value)
                }

                return Ok(Output::Value(Value::new_collection(out)));
            }
            Value::Complex(ComplexValue::Map(m)) => {
                let mut out = m.value.clone();
                for argument in arguments[1..].iter() {
                    let value = argument.evaluate(context)?;
                    match value {
                        Value::Complex(ComplexValue::Map(a)) => {
                            if a.value.len() > 1 {
                                return Err(Error::new(
                                    ErrorKind::InvalidArguments,
                                    format!("Map should contain only 1 key / value pair when calling append"),
                                ));
                            }

                            out.extend(a.value.clone())
                        }
                        _ => {
                            return Err(Error::new(
                                ErrorKind::InvalidArguments,
                                format!("Invalid type to append to map. Expected map"),
                            ))
                        }
                    }
                }

                return Ok(Output::Value(Value::new_map(out)));
            }
            _ => Err(Error::new(
                ErrorKind::InvalidArguments,
                format!("Invalid type for append. Expected collection or map"),
            )),
        }
    }
}
