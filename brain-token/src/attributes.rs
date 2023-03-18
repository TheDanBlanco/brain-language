pub const TOKEN: &str = "token";
pub const REGEX: &str = "regex";

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<T> {
    pub token: T,
    pub literal: String,
}

impl<T> Literal<T> {
    pub fn new(token: T, literal: String) -> Self {
        Literal { token, literal }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Regex<T> {
    pub token: T,
    pub regex: String,
}

impl<T> Regex<T> {
    pub fn new(token: T, regex: String) -> Self {
        Regex { token, regex }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Attribute<T> {
    Literal(Literal<T>),
    Regex(Regex<T>),
}

impl<T> Attribute<T> {
    pub fn new(token: T, attribute: String, string: String) -> Self {
        match attribute.as_str() {
            TOKEN => Attribute::Literal(Literal::new(token, string)),
            REGEX => Attribute::Regex(Regex::new(token, string)),
            _ => panic!("Invalid attribute"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum Test {
        A,
        B,
    }

    #[test]
    fn new_literal() {
        let attribute = Attribute::new(Test::A, "token".to_string(), "test".to_string());

        assert_eq!(attribute, Attribute::Literal(Literal { token: Test::A, literal: "test".to_string() }));
    }

    #[test]
    fn new_regex() {
        let attribute = Attribute::new(Test::B, "regex".to_string(), "[a-z]".to_string());

        assert_eq!(attribute, Attribute::Regex(Regex { token: Test::B, regex: "[a-z]".to_string() }));
    }
}