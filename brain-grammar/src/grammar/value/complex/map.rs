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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::value::Value;

    #[test]
    fn test_map() {
        let mut map = BTreeMap::new();
        map.insert(
            Value::new_string("key".to_string()),
            Value::new_string("value".to_string()),
        );
        let map = Map::new(map);
        assert_eq!(map.to_string(), "{ key: value }");
    }

    #[test]
    fn test_map_empty() {
        let map = Map::new(BTreeMap::new());
        assert_eq!(map.to_string(), "{}");
    }
}
