use core::fmt;
use std::fmt::{Display, Formatter};

use self::collection::Collection;
use self::enumdefinition::EnumDefinition;
use self::function::Function;
use self::map::Map;
use self::r#enum::Enum;
use self::tuple::Tuple;

pub mod collection;
pub mod r#enum;
pub mod enumdefinition;
pub mod function;
pub mod map;
pub mod tuple;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ComplexValue {
    Map(Map),
    Collection(Collection),
    Function(Function),
    Enum(Enum),
    EnumDefinition(EnumDefinition),
    Tuple(Tuple),
}

impl Display for ComplexValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ComplexValue::Map(map) => write!(f, "{}", map),
            ComplexValue::Collection(collection) => write!(f, "{}", collection),
            ComplexValue::Function(function) => write!(f, "{}", function),
            ComplexValue::Enum(r#enum) => write!(f, "{}", r#enum),
            ComplexValue::EnumDefinition(enum_definition) => write!(f, "{}", enum_definition),
            ComplexValue::Tuple(tuple) => write!(f, "{}", tuple),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::grammar::statements::Statement;

    use super::*;

    #[test]
    fn test_display() {
        let map = Map::new(BTreeMap::new());
        let collection = Collection::new(vec![]);
        let function = Function::new(vec![], Statement::new_break());
        let r#enum = Enum::new("test".to_string(), "one".to_string());
        let enum_definition = EnumDefinition::new(
            "test".to_string(),
            vec!["one".to_string(), "two".to_string()],
        );
        let tuple = Tuple::new(vec![]);

        assert_eq!(format!("{map}"), "{}");
        assert_eq!(format!("{collection}"), "[]");
        assert_eq!(format!("{function}"), "[function]");
        assert_eq!(format!("{enum}"), "test::one");
        assert_eq!(format!("{enum_definition}"), "enum test { one, two }");
        assert_eq!(format!("{tuple}"), "()");
    }
}
