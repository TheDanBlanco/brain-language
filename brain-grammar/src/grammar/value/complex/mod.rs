use core::fmt;

use crate::grammar::value::complex::collection::Collection;
use crate::grammar::value::complex::function::Function;
use crate::grammar::value::complex::map::Map;

pub mod collection;
pub mod function;
pub mod map;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ComplexValue {
    Map(Map),
    Collection(Collection),
    Function(Function),
}

impl fmt::Display for ComplexValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComplexValue::Map(map) => write!(f, "{}", map),
            ComplexValue::Collection(collection) => write!(f, "{}", collection),
            ComplexValue::Function(function) => write!(f, "{}", function),
        }
    }
}
