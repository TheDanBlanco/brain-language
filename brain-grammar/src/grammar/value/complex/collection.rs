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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let collection = Collection::new(vec![
            Value::new_number(1),
            Value::new_number(2),
            Value::new_number(3),
        ]);
        assert_eq!(collection.to_string(), "[1, 2, 3]");
    }

    #[test]
    fn test_display_empty() {
        let collection = Collection::new(vec![]);
        assert_eq!(collection.to_string(), "[]");
    }
}
