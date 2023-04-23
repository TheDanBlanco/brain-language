use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Enum {
    pub name: String,
    pub variant: String,
}

impl Enum {
    pub fn new(name: String, variant: String) -> Self {
        Self { name, variant }
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.name, self.variant)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let e = Enum::new("Foo".to_string(), "Bar".to_string());
        assert_eq!(e.to_string(), "Foo::Bar");
    }
}
