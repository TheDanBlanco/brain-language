use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Break,
    Continue,
    None,
    Value(Value),
}
