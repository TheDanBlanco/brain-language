use std::collections::{BTreeMap};

use super::value::Value;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Context {
    pub symbols: BTreeMap<String, Value>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            symbols: BTreeMap::new(),
        }
    }
}