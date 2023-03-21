use std::collections::BTreeMap;

use super::{expressions::Expression, value::Value, Evaluate};

type Symbols = BTreeMap<String, Value>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Context {
    pub symbols: Symbols,
}

impl Context {
    pub fn new() -> Self {
        Context {
            symbols: BTreeMap::new(),
        }
    }

    pub fn clone_and_merge_symbols(
        &mut self,
        arguments: &mut dyn Iterator<Item = (&String, Expression)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut cloned_symbols = self.symbols.clone();

        for (identifier, value) in arguments {
            let val = value.evaluate(self)?;
            cloned_symbols.insert(identifier.to_string(), val);
        }

        Ok(Context {
            symbols: cloned_symbols,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn context_new() {
        let context = Context::new();

        assert_eq!(
            context,
            Context {
                symbols: BTreeMap::new()
            }
        )
    }

    #[test]
    fn clone_and_merge_symbols() {
        let context = &mut Context::new();
        context.symbols.insert("x".into(), Value::Number(0));

        let strings = ["x".to_string()];
        let expressions = [Expression::new_literal(Value::Number(1))];
        let mut zipped = strings.iter().zip(expressions);

        let cloned_context = context.clone_and_merge_symbols(&mut zipped);

        assert_ne!(
            context.symbols.get("x".into()).unwrap(),
            cloned_context.unwrap().symbols.get("x".into()).unwrap(),
        );
    }
}
