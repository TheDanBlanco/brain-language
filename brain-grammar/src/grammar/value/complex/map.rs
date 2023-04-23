use crate::grammar::value::Value;
use core::fmt;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Map {
    pub value: BTreeMap<Value, Value>,
}

impl Map {
    pub fn new(value: BTreeMap<Value, Value>) -> Self {
        Self { value }
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut output = String::new();
        for (i, (key, value)) in self.value.iter().enumerate() {
            output.push_str(&format!("{}: {}", key, value));
            if i != self.value.len() - 1 {
                output.push_str(", ");
            }
        }
        if output.is_empty() {
            return write!(f, "{{}}");
        }

        write!(f, "{{ {output} }}")
    }
}
