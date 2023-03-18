use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::lang::grammar::{
    context::Context, expressions::Expression, output::Output, Node, Resolve,
};

use self::{
    assignment::Assignment, block::Block, conditional::Conditional,
    functiondefinition::FunctionDefinition, r#break::Break, r#continue::Continue, r#for::For,
    r#loop::Loop, r#return::Return, reassignment::Reassignment,
};

use super::{token::BrainToken, Match, Parse};

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

    pub fn new_conditional(
        expression: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Self {
        Self::Conditional(Conditional::new(expression, consequence, alternative))
    }

    pub fn new_function_definition(
        identifier: String,
        arguments: Vec<String>,
        block: Statement,
    ) -> Self {
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

impl Resolve for Statement {
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

impl Parse for Statement {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.peek();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected statement, found unexpected End of File".to_string(),
            ));
        }

        let token = &next.unwrap().token;

        let statement = match token {
            BrainToken::Let => Statement::Assignment(Assignment::parse(stream)?),
            BrainToken::Function => Statement::FunctionDefinition(FunctionDefinition::parse(stream)?),
            BrainToken::If => Statement::Conditional(Conditional::parse(stream)?),
            BrainToken::Identifier => Statement::Reassignment(Reassignment::parse(stream)?),
            BrainToken::Loop => Statement::Loop(Loop::parse(stream)?),
            BrainToken::For => Statement::For(For::parse(stream)?),
            BrainToken::LeftBrace => Statement::Block(Block::parse(stream)?),
            BrainToken::Break => Statement::Break(Break::parse(stream)?),
            BrainToken::Continue => Statement::Continue(Continue::parse(stream)?),
            BrainToken::Return => Statement::Return(Return::parse(stream)?),
            _ => return Err(
                Error::new(
                ErrorKind::UnexpectedToken,
                format!("Expected let, fn, if, an identifier, loop, for, {{, break, continue, or return, found {token}")
                )
            )
        };

        Ok(statement)
    }
}

impl Match for Statement {
    fn matches(token: &BrainToken) -> bool {
        matches!(
            token,
            BrainToken::Let
                | BrainToken::Function
                | BrainToken::If
                | BrainToken::Identifier
                | BrainToken::Loop
                | BrainToken::For
                | BrainToken::LeftBrace
                | BrainToken::Break
                | BrainToken::Continue
                | BrainToken::Return
        )
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::{expressions::operator::Operator, value::Value};

    use super::*;

    #[test]
    fn new_statement_assignment() {
        let statement =
            Statement::new_assignment("foo".to_string(), Expression::new_literal(Value::Number(0)));

        assert_eq!(
            statement,
            Statement::Assignment(Assignment::new(
                "foo".to_string(),
                Expression::new_literal(Value::Number(0))
            ))
        );
    }

    #[test]
    fn eval_statement_assignment() {
        let context = &mut Context::new();
        let statement =
            Statement::new_assignment("foo".to_string(), Expression::new_literal(Value::Number(0)));
        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_assignment() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Let, None),
            Token::new(4..5, BrainToken::Identifier, Some("x".to_string())),
            Token::new(5..6, BrainToken::Assign, None),
            Token::new(7..8, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok());
    }

    #[test]
    fn new_statement_reassignment() {
        let statement = Statement::new_reassignment(
            "foo".to_string(),
            Expression::new_literal(Value::Number(0)),
        );

        assert_eq!(
            statement,
            Statement::Reassignment(Reassignment::new(
                "foo".to_string(),
                Expression::new_literal(Value::Number(0))
            ))
        );
    }

    #[test]
    fn eval_statement_reassignment() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));

        let statement = Statement::new_reassignment(
            "foo".to_string(),
            Expression::new_literal(Value::Number(0)),
        );
        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_reassignement() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("x".to_string())),
            Token::new(1..2, BrainToken::Assign, None),
            Token::new(3..4, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok());
    }

    #[test]
    fn new_statement_conditional() {
        let statement = Statement::new_conditional(
            Expression::new_literal(Value::Boolean(true)),
            Statement::new_break(),
            Some(Statement::new_break()),
        );

        assert_eq!(
            statement,
            Statement::Conditional(Conditional::new(
                Expression::new_literal(Value::Boolean(true)),
                Statement::new_break(),
                Some(Statement::new_break())
            ))
        );
    }

    #[test]
    fn eval_statement_conditional() {
        let context = &mut Context::new();

        let statement = Statement::new_conditional(
            Expression::new_literal(Value::Boolean(true)),
            Statement::new_break(),
            Some(Statement::new_break()),
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_conditional() {
        let tokens = vec![
            Token::new(0..2, BrainToken::If, None),
            Token::new(3..4, BrainToken::True, None),
            Token::new(5..6, BrainToken::LeftBrace, None),
            Token::new(7..11, BrainToken::Break, None),
            Token::new(12..13, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_function_definition() {
        let statement = Statement::new_function_definition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Statement::new_return(Expression::new_binary(
                Expression::new_identifier("a".to_string()),
                Operator::new_addition(),
                Expression::new_identifier("b".to_string()),
            )),
        );

        assert_eq!(
            statement,
            Statement::FunctionDefinition(FunctionDefinition::new(
                "adder".into(),
                vec!["a".into(), "b".into()],
                Statement::new_return(Expression::new_binary(
                    Expression::new_identifier("a".to_string()),
                    Operator::new_addition(),
                    Expression::new_identifier("b".to_string()),
                ))
            ))
        );
    }

    #[test]
    fn eval_statement_function_definition() {
        let context = &mut Context::new();

        let statement = Statement::new_function_definition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Statement::new_return(Expression::new_binary(
                Expression::new_identifier("a".to_string()),
                Operator::new_addition(),
                Expression::new_identifier("b".to_string()),
            )),
        );

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_function_definition() {
        let tokens = vec![
            Token::new(0..3, BrainToken::Function, None),
            Token::new(4..8, BrainToken::Identifier, Some("adder".to_string())),
            Token::new(8..9, BrainToken::LeftParen, None),
            Token::new(9..10, BrainToken::Identifier, Some("a".to_string())),
            Token::new(10..11, BrainToken::Comma, None),
            Token::new(12..13, BrainToken::Identifier, Some("b".to_string())),
            Token::new(13..14, BrainToken::RightParen, None),
            Token::new(15..16, BrainToken::LeftBrace, None),
            Token::new(17..21, BrainToken::Return, None),
            Token::new(22..23, BrainToken::Identifier, Some("a".to_string())),
            Token::new(24..25, BrainToken::Plus, None),
            Token::new(26..27, BrainToken::Identifier, Some("b".to_string())),
            Token::new(27..28, BrainToken::Semicolon, None),
            Token::new(28..29, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_return() {
        let statement = Statement::new_return(Expression::new_literal(Value::Number(0)));

        assert_eq!(
            statement,
            Statement::Return(Return::new(Expression::new_literal(Value::Number(0))))
        );
    }

    #[test]
    fn eval_statement_return() {
        let context = &mut Context::new();

        let statement = Statement::new_return(Expression::new_literal(Value::Number(0)));

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_return() {
        let tokens = vec![
            Token::new(0..6, BrainToken::Return, None),
            Token::new(7..8, BrainToken::Number, Some("0".to_string())),
            Token::new(8..9, BrainToken::Semicolon, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_break() {
        let statement = Statement::new_break();

        assert_eq!(statement, Statement::Break(Break {}));
    }

    #[test]
    fn eval_statement_break() {
        let context = &mut Context::new();

        let statement = Statement::new_break();

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_break() {
        let tokens = vec![Token::new(0..5, BrainToken::Break, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_continue() {
        let statement = Statement::new_continue();

        assert_eq!(statement, Statement::Continue(Continue {}));
    }

    #[test]
    fn eval_statement_continue() {
        let context = &mut Context::new();

        let statement = Statement::new_continue();

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_continue() {
        let tokens = vec![Token::new(0..7, BrainToken::Continue, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_loop() {
        let statement = Statement::new_loop(Statement::new_break());

        assert_eq!(
            statement,
            Statement::Loop(Loop::new(Statement::new_break()))
        );
    }

    #[test]
    fn eval_statement_loop() {
        let context = &mut Context::new();

        let statement = Statement::new_loop(Statement::new_break());

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_loop() {
        let tokens = vec![
            Token::new(0..4, BrainToken::Loop, None),
            Token::new(5..6, BrainToken::LeftBrace, None),
            Token::new(7..11, BrainToken::Break, None),
            Token::new(11..12, BrainToken::Semicolon, None),
            Token::new(12..13, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

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
            Statement::For(For::new(
                "item".into(),
                Expression::new_literal(Value::Collection(vec![])),
                Statement::new_break(),
            ))
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
    fn parse_statement_for() {
        let tokens = vec![
            Token::new(0..3, BrainToken::For, None),
            Token::new(4..8, BrainToken::Identifier, Some("item".to_string())),
            Token::new(9..10, BrainToken::In, None),
            Token::new(11..12, BrainToken::LeftBracket, None),
            Token::new(12..13, BrainToken::RightBracket, None),
            Token::new(14..15, BrainToken::LeftBrace, None),
            Token::new(16..20, BrainToken::Break, None),
            Token::new(20..21, BrainToken::Semicolon, None),
            Token::new(21..22, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn new_statement_block() {
        let statement = Statement::new_block(vec![Node::from_statement(Statement::new_break())]);

        assert_eq!(
            statement,
            Statement::Block(Block::new(vec![Node::from_statement(
                Statement::new_break(),
            )]))
        );
    }

    #[test]
    fn eval_statement_block() {
        let context = &mut Context::new();

        let statement = Statement::new_block(vec![Node::from_statement(Statement::new_break())]);

        let result = statement.resolve(context);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_block() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(2..6, BrainToken::Break, None),
            Token::new(6..7, BrainToken::Semicolon, None),
            Token::new(7..8, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_ok())
    }

    #[test]
    fn parse_statement_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected statement, found unexpected End of File".to_string()
        )
    }

    #[test]
    fn parse_statement_pass_invalid_token() {
        let tokens = vec![Token::new(0..1, BrainToken::Plus, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Statement::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected let, fn, if, an identifier, loop, for, {, break, continue, or return, found Token::Plus".to_string()
        )
    }
}
