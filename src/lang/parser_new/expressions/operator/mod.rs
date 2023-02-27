use crate::lang::parser_new::{value::Value, context::Context};

use self::{mathematical::Mathematical, logical::Logical, comparison::Comparison};

pub mod mathematical;
pub mod logical;
pub mod comparison;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Mathematical(Mathematical),
    Logical(Logical),
    Comparison(Comparison),
}

impl Operator {
    pub fn eval(&self, left: Value, right: Value, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>>{
        match self {
            Operator::Mathematical(mathematical) => mathematical.eval(left, right, context),
            Operator::Logical(logical) => logical.eval(left, right, context),
            Operator::Comparison(comparison) => comparison.eval(left, right, context),
        }
    }
}