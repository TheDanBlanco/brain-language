use crate::lang::parser_new::{expressions::expression::{Evaluatable, Expression}, context::Context, value::Value, error::{ErrorKind, Error}};

pub struct Index {
    index: Box<Expression>,
    target: Box<Expression>,
}

impl Index {
    pub fn new(index: Expression, target: Expression) -> Self {
        Index {
            index: Box::new(index),
            target: Box::new(target),
        }
    }
}

impl Evaluatable for Index {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let target = self.target.eval(context)?;

        match target {
            Value::Collection(collection) => {
                let index = self.index.eval(context)?;

                match index {
                    Value::Number(index) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!("Index {} is out of bounds", index),
                            ));
                        }

                        if index >= (collection.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!("Index {} is out of bounds", index),
                            ));
                        }

                        Ok(collection[index as usize].clone())
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be an integer, not {index}"),
                    )),
                }
            }
            Value::Map(map) => {
                let key = self.index.eval(context)?;

                match map.get(&key) {
                    Some(value) => Ok(value.clone()),
                    None => Err(Error::new(
                        ErrorKind::KeyNotFound,
                        format!("Key {} not found", key),
                    )),
                }
            }
            Value::String(string) => {
                let index = self.index.eval(context)?;

                match index {
                    Value::Number(index) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!("Index {} is out of bounds", index),
                            ));
                        }

                        if index >= (string.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!("Index {} is out of bounds", index),
                            ));
                        }

                        Ok(Value::String(string.chars().nth(index as usize).unwrap().to_string()))
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be an integer, not {index}"),
                    )),
                }
            }
            _ => Err(Error::new(
                ErrorKind::InvalidType,
                format!("Cannot index into {target}"),
            )),
        }
    }
}