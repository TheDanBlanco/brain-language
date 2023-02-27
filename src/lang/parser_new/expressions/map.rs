use std::{collections::BTreeMap};

use crate::lang::parser_new::{value::Value, context::Context, error::{ErrorKind, Error}};

use super::expression::{Evaluatable, Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Map {
    pub fn new(pairs: Vec<(Expression, Expression)>) -> Self {
        Map { pairs }
    }
}

impl Evaluatable for Map {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut map = BTreeMap::new();

        for (key, value) in &self.pairs {
            let key = key.eval(context)?;
            //only accept number, strings, or bools as keys.
            match key {
                Value::Number(_) | Value::String(_) | Value::Boolean(_) => {}
                _ => {
                    return Err(Error::new(
                        ErrorKind::InvalidMapKey,
                        format!("Invalid map key: {key}",),
                    ))
                }
            }

            let value = value.eval(context)?;
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Map(map))
    }
}