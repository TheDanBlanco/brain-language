use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct EnumDefinition {
    pub name: String,
    pub variants: Vec<String>,
}

impl EnumDefinition {
    pub fn new(name: String, variants: Vec<String>) -> Self {
        Self { name, variants }
    }
}

impl Display for EnumDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {} {{ ", self.name)?;
        for (i, variant) in self.variants.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", variant)?;
        }
        write!(f, " }}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let enum_definition = EnumDefinition::new(
            "Test".to_string(),
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
        );
        assert_eq!(enum_definition.to_string(), "enum Test { A, B, C }");
    }
}
