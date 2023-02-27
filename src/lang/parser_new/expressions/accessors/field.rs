use crate::lang::parser_new::{expressions::expression::{Evaluatable, Expression}, context::Context, value::Value, error::{ErrorKind, Error}};


pub struct Field {
    field: String,
    target: Box<Expression>,
}

impl Field {
    pub fn new(field: String, target: Expression) -> Self {
        Field {
            field,
            target: Box::new(target),
        }
    }
}

impl Evaluatable for Field {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<&Value, Box<dyn std::error::Error>> {
        let target = self.target.eval(context)?;

        match target {
            Value::Map(map) => {
                match map.get(&Value::String(self.field.clone())) {
                    Some(value) => Ok(&value),
                    None => Err(Error::new(
                        ErrorKind::KeyNotFound,
                        format!("Key {} not found", self.field),
                    )),
                }
            }
            _ => Err(Error::new(
                ErrorKind::InvalidType,
                format!("Cannot access field {} of {target}", self.field),
            )),
        }
    }
}
