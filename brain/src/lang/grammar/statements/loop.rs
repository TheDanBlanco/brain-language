use crate::lang::grammar::{context::Context, output::Output, Resolveable};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loop {
    statement: Box<Statement>,
}

impl Loop {
    pub fn new(statement: Statement) -> Self {
        Loop {
            statement: Box::new(statement),
        }
    }
}

impl Resolveable for Loop {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        loop {
            let out = self.statement.resolve(context)?;

            if matches!(out, Output::Break) {
                break Ok(out);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::Node;

    use super::*;

    #[test]
    fn new_loop() {
        let statement = Statement::new_break();
        let r#loop = Loop::new(statement);

        assert_eq!(
            r#loop,
            Loop {
                statement: Box::new(Statement::new_break())
            }
        )
    }

    #[test]
    fn resolve_loop_with_break() {
        let context = &mut Context::new();
        let nodes = vec![Node::from_statement(Statement::new_break())];

        let block = Statement::new_block(nodes);
        let r#loop = Loop::new(block);

        let result = r#loop.resolve(context);

        assert!(result.is_ok());

        assert_eq!(result.unwrap(), Output::Break);
    }
}
