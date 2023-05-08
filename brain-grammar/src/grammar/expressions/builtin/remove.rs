use crate::grammar::{
    context::Context,
    expressions::Expression,
    output::Output,
    value::{complex::ComplexValue, Value},
    Evaluate,
};

use brain_error::{Error, ErrorKind};

pub const REMOVE: &str = "remove";

pub struct Remove;

impl Remove {
    pub fn resolve(
        &self,
        context: &mut Context,
        arguments: Vec<Expression>,
    ) -> Result<Output, Box<dyn std::error::Error>> {
        if arguments.len() < 2 {
            return Err(Error::new(
                ErrorKind::InvalidArguments,
                "Cannot call remove with less than 2 arguments".to_string(),
            ));
        }

        let input = arguments.first().unwrap().evaluate(context)?;

        match input {
            Value::Complex(ComplexValue::Collection(c)) => {
                let mut out = c.value.clone();
                for argument in arguments[1..].iter() {
                    let value = argument.evaluate(context)?;
                    let item = value.clone();
                    if let Some(index) = out.iter().position(|i| *i == item) {
                        out.remove(index); 
                    }
                }

                return Ok(Output::Value(Value::new_collection(out)));
            }
            Value::Complex(ComplexValue::Map(m)) => {
                let mut out = m.value.clone();
                for argument in arguments[1..].iter() {
                    let value = argument.evaluate(context)?;
                    match value {
                        Value::Complex(ComplexValue::Map(a)) => {
                            out.extend(a.value.clone())
                        }
                        _ => {
                            return Err(Error::new(
                                ErrorKind::InvalidArguments,
                                "Invalid type for remove to map. Expected map".to_string(),
                            ))
                        }
                    }
                }

                return Ok(Output::Value(Value::new_map(out)));
            }
            _ => Err(Error::new(
                ErrorKind::InvalidArguments,
                "Invalid type for remove. Expected collection or map".to_string(),
            )),
        }
    }
}
