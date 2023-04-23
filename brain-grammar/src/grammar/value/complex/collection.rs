use core::fmt;

use crate::grammar::value::Value;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Collection {
    pub value: Vec<Value>,
}

impl Collection {
    pub fn new(value: Vec<Value>) -> Self {
        Self { value }
    }
}

impl fmt::Display for Collection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for (i, value) in self.value.iter().enumerate() {
            if i != 0 {
                output.push_str(", ");
            }
            output.push_str(&value.to_string());
        }
        write!(f, "[{}]", output)
    }
}
