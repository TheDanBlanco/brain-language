use crate::lang::grammar::{
    context::Context, expressions::Expression, output::Output, Node, Resolveable,
};

use self::{
    assignment::Assignment, block::Block, conditional::Conditional,
    functiondefinition::FunctionDefinition, r#break::Break, r#continue::Continue, r#for::For,
    r#loop::Loop, r#return::Return, reassignment::Reassignment,
};

pub mod assignment;
pub mod block;
pub mod r#break;
pub mod conditional;
pub mod r#continue;
pub mod r#for;
pub mod functiondefinition;
pub mod r#loop;
pub mod reassignment;
pub mod r#return;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Assignment(Assignment),
    Reassignment(Reassignment),
    Conditional(Conditional),
    FunctionDefinition(FunctionDefinition),
    Return(Return),
    Break(Break),
    Continue(Continue),
    Loop(Loop),
    For(For),
    Block(Block),
}

impl Statement {
    pub fn new_assignment(target: String, value: Expression) -> Self {
        Self::Assignment(Assignment::new(target, value))
    }

    pub fn new_reassignment(target: String, value: Expression) -> Self {
        Self::Reassignment(Reassignment::new(target, value))
    }

    pub fn new_conditional(expression: Expression, consequence: Statement, alternative: Option<Statement>) -> Self {
        Self::Conditional(Conditional::new(expression, consequence, alternative))
    }

    pub fn new_function_definition(identifier: String, arguments: Vec<String>, block: Statement) -> Self {
        Self::FunctionDefinition(FunctionDefinition::new(identifier, arguments, block))
    }

    pub fn new_return(value: Expression) -> Self {
        Self::Return(Return::new(value))
    }

    pub fn new_break() -> Self {
        Self::Break(Break)
    }

    pub fn new_continue() -> Self {
        Self::Continue(Continue)
    }

    pub fn new_loop(statement: Statement) -> Self {
        Self::Loop(Loop::new(statement))
    }

    pub fn new_for(identifier: String, iterable: Expression, block: Statement) -> Self {
        Self::For(For::new(identifier, iterable, block))
    }

    pub fn new_block(nodes: Vec<Node>) -> Self {
        Self::Block(Block::new(nodes))
    }
}

impl Resolveable for Statement {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        match self {
            Statement::Assignment(assignment) => assignment.resolve(context),
            Statement::Reassignment(assignment) => assignment.resolve(context),
            Statement::Break(r#break) => r#break.resolve(context),
            Statement::Continue(r#continue) => r#continue.resolve(context),
            Statement::Block(block) => block.resolve(context),
            Statement::Return(r#return) => r#return.resolve(context),
            Statement::Loop(r#loop) => r#loop.resolve(context),
            Statement::For(r#for) => r#for.resolve(context),
            Statement::Conditional(conditional) => r#conditional.resolve(context),
            Statement::FunctionDefinition(definition) => definition.resolve(context),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::{value::Value, expressions::operator::{mathematical::Mathematical, Operator}};

    use super::*;

    #[test]
    fn new_statement_assignment() {
        let statement = Statement::new_assignment("foo".to_string(), Expression::new_literal(Value::Number(0)));

        assert_eq!(
            statement,
            Statement::Assignment(Assignment::new("foo".to_string(), Expression::new_literal(Value::Number(0))))
        );
    }

    #[test]
    fn eval_statement_assignment() {
        let context = &mut Context::new();
        let statement = Statement::new_assignment("foo".to_string(), Expression::new_literal(Value::Number(0)));
        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_reassignment() {
        let statement = Statement::new_reassignment("foo".to_string(), Expression::new_literal(Value::Number(0)));

        assert_eq!(
            statement,
            Statement::Reassignment(Reassignment::new("foo".to_string(), Expression::new_literal(Value::Number(0))))
        );
    }

    #[test]
    fn eval_statement_reassignment() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));

        let statement = Statement::new_reassignment("foo".to_string(), Expression::new_literal(Value::Number(0)));
        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_conditional() {
        let statement = Statement::new_conditional(
            Expression::new_literal(Value::Boolean(true)),
            Statement::new_break(),
            Some(Statement::new_break())
        );

        assert_eq!(
            statement,
            Statement::Conditional(
                Conditional::new(
                    Expression::new_literal(Value::Boolean(true)),
                    Statement::new_break(),
                    Some(Statement::new_break())
                )
            )
        );
    }

    #[test]
    fn eval_statement_conditional() {
        let context = &mut Context::new();

        let statement = Statement::new_conditional(
            Expression::new_literal(Value::Boolean(true)),
            Statement::new_break(),
            Some(Statement::new_break())
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_function_definition() {
        let statement = Statement::new_function_definition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Statement::new_return(
                Expression::new_binary(
                    Expression::new_identifier("a".to_string()), 
                    Operator::Mathematical(Mathematical::Add), 
                    Expression::new_identifier("b".to_string()), 
                )
            )
        );

        assert_eq!(
            statement,
            Statement::FunctionDefinition(
                FunctionDefinition::new(
                    "adder".into(),
                    vec!["a".into(), "b".into()],
                    Statement::new_return(
                        Expression::new_binary(
                            Expression::new_identifier("a".to_string()), 
                            Operator::Mathematical(Mathematical::Add), 
                            Expression::new_identifier("b".to_string()), 
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn eval_statement_function_definition() {
        let context = &mut Context::new();

        let statement = Statement::new_function_definition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Statement::new_return(
                Expression::new_binary(
                    Expression::new_identifier("a".to_string()), 
                    Operator::Mathematical(Mathematical::Add), 
                    Expression::new_identifier("b".to_string()), 
                )
            )
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_return() {
        let statement = Statement::new_return(
            Expression::new_literal(Value::Number(0))
        );

        assert_eq!(
            statement,
            Statement::Return(
                Return::new(
                    Expression::new_literal(Value::Number(0))
                )
            )
        );
    }

    #[test]
    fn eval_statement_return() {
        let context = &mut Context::new();

        let statement = Statement::new_return(
            Expression::new_literal(Value::Number(0))
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_break() {
        let statement = Statement::new_break();

        assert_eq!(
            statement,
            Statement::Break(
                Break{}
            )
        );
    }

    #[test]
    fn eval_statement_break() {
        let context = &mut Context::new();

        let statement = Statement::new_break();

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_continue() {
        let statement = Statement::new_continue();

        assert_eq!(
            statement,
            Statement::Continue(
                Continue{}
            )
        );
    }

    #[test]
    fn eval_statement_continue() {
        let context = &mut Context::new();

        let statement = Statement::new_continue();

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_loop() {
        let statement = Statement::new_loop(
            Statement::new_break(),
        );

        assert_eq!(
            statement,
            Statement::Loop(
                Loop::new(
                    Statement::new_break()
                )
            )
        );
    }

    #[test]
    fn eval_statement_loop() {
        let context = &mut Context::new();

        let statement = Statement::new_loop(
            Statement::new_break(),
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_for() {
        let statement = Statement::new_for(
            "item".into(),
            Expression::new_literal(Value::Collection(vec![])),
            Statement::new_break(),
        );

        assert_eq!(
            statement,
            Statement::For(
                For::new(
                    "item".into(),
                    Expression::new_literal(Value::Collection(vec![])),
                    Statement::new_break(),
                )
            )
        );
    }

    #[test]
    fn eval_statement_for() {
        let context = &mut Context::new();

        let statement = Statement::new_for(
            "item".into(),
            Expression::new_literal(Value::Collection(vec![])),
            Statement::new_break(),
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_block() {
        let statement = Statement::new_block(
            vec![
                Node::from_statement(
                    Statement::new_break(),
                )
            ]
        );

        assert_eq!(
            statement,
            Statement::Block(
                Block::new(
                    vec![
                        Node::from_statement(
                            Statement::new_break(),
                        )
                    ]
                )
            )
        );
    }

    #[test]
    fn eval_statement_block() {
        let context = &mut Context::new();

        let statement = Statement::new_block(
            vec![
                Node::from_statement(
                    Statement::new_break(),
                )
            ]
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }
}