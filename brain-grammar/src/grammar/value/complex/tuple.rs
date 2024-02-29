use crate::grammar::value::Value;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Tuple {
    pub values: Vec<Value>,
}

impl Tuple {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values }
    }
}

impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        s.push('(');

        for (i, value) in self.values.iter().enumerate() {
            s.push_str(&format!("{}", value));

            if i < self.values.len() - 1 {
                s.push_str(", ");
            }
        }

        s.push(')');

        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tuple() {
        let tuple = Tuple::new(vec![Value::new_number(1), Value::new_number(2)]);

        assert_eq!(format!("{}", tuple), "(1, 2)");
    }
}
