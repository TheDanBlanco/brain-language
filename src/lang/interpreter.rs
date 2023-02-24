use std::collections::{BTreeMap, HashMap};

use crate::lang::parser::{Accessor, LogicalOperator, MathematicalOperator};

use super::parser::{
    ComparisonOperator, Expression, Operator, Statement, StatementExpression, Value,
};

// enum for the different types of interpreter return values
#[derive(Debug, PartialEq)]
pub enum InterpreterReturn {
    Value(Value),
    Break,
    Continue,
    None,
}

fn parse_expression(expression: Expression, symbols: &mut HashMap<String, Value>) -> Value {
    match expression {
        Expression::Literal(value) => return value,
        Expression::Binary(lhs, operator, rhs) => {
            let lhs = parse_expression(*lhs, symbols);
            let rhs = parse_expression(*rhs, symbols);

            return match operator {
                Operator::MathematicalOperator(op) => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        MathematicalOperator::Plus => Value::Number(lhs + rhs),
                        MathematicalOperator::Minus => Value::Number(lhs - rhs),
                        MathematicalOperator::Times => Value::Number(lhs * rhs),
                        MathematicalOperator::Divide => Value::Number(lhs / rhs),
                    },
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    (Value::String(lhs), Value::Number(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    (Value::Number(lhs), Value::String(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    _ => panic!("cannot perform mathematical operators on types other than Number and String")
                },
                Operator::ComparisonOperator(op) => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs <= rhs),
                        ComparisonOperator::NotEqual => Value::Boolean(lhs != rhs),
                    }
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs <= rhs),
                        ComparisonOperator::NotEqual => Value::Boolean(lhs != rhs),
                    }
                    _ => panic!("cannot perform comparison on types other than Number and Number or String and String")
                }
                Operator::LogicalOperator(op) => match (lhs, rhs) {
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(lhs && rhs),
                        LogicalOperator::Or => Value::Boolean(lhs || rhs),
                    },
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(!lhs.is_empty() && !rhs.is_empty()),
                        LogicalOperator::Or => Value::Boolean(!lhs.is_empty() || !rhs.is_empty()),
                    }
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(lhs != 0 && rhs != 0),
                        LogicalOperator::Or => Value::Boolean(lhs != 0 || rhs != 0),
                    }
                    _ => panic!("cannot perform logical operator on type Function or Null")
                }
            };
        }
        Expression::FunctionCall(identifier, incoming_arguments) => {
            let builtin_return =
                check_builtin_functions(*identifier.clone(), incoming_arguments.clone(), symbols);

            if builtin_return.is_some() {
                return builtin_return.unwrap();
            }

            let function = parse_expression(*identifier.clone(), symbols);

            if let Value::Function(defined_arguments, definition) = function {
                let mut local_symbols = symbols.clone();
                for (identifier, value) in defined_arguments.iter().zip(incoming_arguments) {
                    let val = parse_expression(value, &mut local_symbols.clone());
                    local_symbols.insert(identifier.to_string(), val);
                }
                if let InterpreterReturn::Value(return_value) =
                    parse_block(definition.to_owned(), &mut local_symbols)
                {
                    return return_value;
                }

                return Value::Null;
            }

            panic!("{function} was not a function");
        }
        Expression::Identifier(identifier) => {
            if let Some(val) = symbols.get(&identifier) {
                return Value::to_owned(val);
            }

            panic!("could not find identifier {identifier}");
        }
        Expression::Collection(collection) => {
            let mut values = Vec::new();
            for value in collection {
                values.push(parse_expression(value, symbols));
            }

            return Value::Collection(values);
        }
        Expression::Map(map) => {
            let mut values = BTreeMap::new();
            for (key, value) in map {
                let key = parse_expression(key, symbols);
                let value = parse_expression(value, symbols);
                values.insert(key, value);
            }

            return Value::Map(values);
        }
        Expression::Accessor(item, accessor) => {
            let item = parse_expression(*item, symbols);
            let accessor = match accessor {
                Accessor::Index(expression) => parse_expression(*expression, symbols),
                Accessor::Property(val) => val,
            };

            if let Value::Collection(collection) = &item {
                if let Value::Number(index) = accessor {
                    if let Some(val) = collection.get(index as usize) {
                        return Value::to_owned(val);
                    }
                }

                panic!("could not find index {accessor} in collection");
            }

            if let Value::Map(map) = &item {
                if let Some(val) = map.get(&accessor) {
                    return Value::to_owned(val);
                }

                panic!("could not find key {accessor} in map");
            }
        }
        _ => println!("couldn't parse expression"),
    }

    Value::Null
}

fn parse_statement(
    statement: Statement,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    match statement {
        Statement::Assignment(identifier, expression) => {
            if symbols.get(&identifier).is_some() {
                panic!("identifier {identifier} already exists")
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);

            InterpreterReturn::None
        }
        Statement::Reassignment(identifier, expression) => {
            if symbols.get(&identifier).is_none() {
                panic!("could not find identifier {identifier}")
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);

            InterpreterReturn::None
        }
        Statement::FunctionDefinition(identifier, arguments, definition) => {
            symbols.insert(identifier, Value::Function(arguments, definition));

            InterpreterReturn::None
        }
        Statement::Conditional(condition, consequence, alternative) => {
            let value = parse_expression(condition, symbols);
            if let Value::Boolean(result) = value {
                if result {
                    return parse_block(consequence, symbols);
                }

                if let Some(alt) = *alternative {
                    return parse_block(Box::new(alt), symbols);
                }

                return InterpreterReturn::None;
            }

            panic!("expected boolean value for conditional statement");
        }
        Statement::Loop(loop_statement) => loop {
            let out = parse_block(loop_statement.clone(), symbols);
            if let InterpreterReturn::Break = out {
                break out;
            }
        },
        Statement::For(identifier, collection, block) => {
            let collection = parse_expression(*collection, symbols);
            if let Value::Collection(values) = collection {
                for value in values {
                    symbols.insert(identifier.to_string(), value);
                    let out = parse_block(block.clone(), symbols);
                    if let InterpreterReturn::Break = out {
                        return out;
                    }
                }
            }

            InterpreterReturn::None
        }
        Statement::Block(_) => {
            return parse_block(Box::new(statement), symbols);
        }
        Statement::Break => InterpreterReturn::Break,
        Statement::Continue => InterpreterReturn::Continue,
        Statement::Return(expression) => {
            InterpreterReturn::Value(parse_expression(*expression, symbols))
        }
    }
}

pub fn evaluate(
    statement_expression: StatementExpression,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    match statement_expression {
        StatementExpression::Expression(expression) => {
            parse_expression(expression, symbols);
            InterpreterReturn::None
        }
        StatementExpression::Statement(statement) => parse_statement(statement, symbols),
    }
}

pub fn interpret(statement_expression: Vec<StatementExpression>) {
    let mut symbols = HashMap::new();
    evaluate_statement_expressions(statement_expression, &mut symbols);
}

pub fn evaluate_statement_expressions(
    statement_expressions: Vec<StatementExpression>,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    for statement_expression in statement_expressions {
        let out = evaluate(statement_expression, symbols);

        if InterpreterReturn::None != out {
            return out;
        }
    }

    InterpreterReturn::None
}

pub fn parse_block(
    statement: Box<Statement>,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    if let Statement::Block(block) = *statement {
        return evaluate_statement_expressions(block, symbols);
    }

    panic!("attempted to parse {statement:#?} as a block");
}

fn check_builtin_functions(
    expression: Expression,
    arguments: Vec<Expression>,
    symbols: &mut HashMap<String, Value>,
) -> Option<Value> {
    if let Expression::Identifier(identifier) = expression {
        match identifier.as_str() {
            "print" => {
                let mut out = String::new();
                for argument in arguments {
                    let value = parse_expression(argument, symbols);
                    out.push_str(&value.to_string());
                }
                println!("{}", out);
                return Some(Value::Null);
            }
            _ => return None,
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::lang::parser::Accessor;

    use super::*;

    #[test]
    fn test_parse_expression_number_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Number(1));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(1));
    }

    #[test]
    fn test_parse_expression_string_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::String("hello".to_string()));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("hello".to_string()));
    }

    #[test]
    fn test_parse_expression_boolean_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Boolean(true));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_null_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Null);
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Null);
    }

    #[test]
    fn test_parse_expression_function_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Function(
            vec!["a".to_string()],
            Box::new(Statement::Block(vec![])),
        ));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(
            value,
            Value::Function(vec!["a".to_string()], Box::new(Statement::Block(vec![])))
        );
    }

    #[test]
    fn test_parse_expression_binary_operator_add_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(3));
    }

    #[test]
    fn test_parse_expression_binary_operator_subtract_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::MathematicalOperator(MathematicalOperator::Minus),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(-1));
    }

    #[test]
    fn test_parse_expression_binary_operator_multiply_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::MathematicalOperator(MathematicalOperator::Times),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(2));
    }

    #[test]
    fn test_parse_expression_binary_operator_divide_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2))),
            Operator::MathematicalOperator(MathematicalOperator::Divide),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(1));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::String("world".to_string()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("helloworld".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_string_and_number() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("hello1".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_number_and_string() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("1hello".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_numbers_and_strings_twice() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1))),
                Operator::MathematicalOperator(MathematicalOperator::Plus),
                Box::new(Expression::Literal(Value::String("hello".to_string()))),
            )),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2))),
                Operator::MathematicalOperator(MathematicalOperator::Plus),
                Box::new(Expression::Literal(Value::String("world".to_string()))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("1hello2world".to_string()));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parsep_expression_comparison_operator_greater_than_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::Number(1))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_or_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".into()))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("hello".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".into()))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("world".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_and_bools() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_and_bools_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Boolean(false))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_or_bools() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_or_bools_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(false))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Boolean(false))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_complex_comparison() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(1))),
            )),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(2))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_complex_comparison_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(1))),
            )),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(3))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_or() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_or_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::String("".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_or() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Number(2))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_or_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Number(0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    #[should_panic]
    fn test_parse_expression_comparison_operator_equal_numbers_and_strings_unsupported() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("1".to_string()))),
        );
        parse_expression(expression, &mut symbols);
    }

    #[test]
    #[should_panic]
    fn test_parse_expression_binary_operator_add_string_and_boolean_unsupported() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        parse_expression(expression, &mut symbols);
    }

    #[test]
    fn test_parse_identifier() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1));
        let expression = Expression::Identifier("a".into());
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(1));
    }

    #[test]
    #[should_panic(expected = "could not find identifier a")]
    fn test_parse_identifier_not_found() {
        let mut symbols = HashMap::new();
        let expression = Expression::Identifier("a".into());
        parse_expression(expression, &mut symbols);
    }

    #[test]
    fn test_parse_statement_assignment() {
        let mut symbols = HashMap::new();
        let statement =
            Statement::Assignment("a".into(), Box::new(Expression::Literal(Value::Number(1))));
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1));
    }

    #[test]
    #[should_panic(expected = "identifier a already exists")]
    fn test_parse_statement_assignment_identifier_already_exists() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1));
        let statement =
            Statement::Assignment("a".into(), Box::new(Expression::Literal(Value::Number(2))));
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2));
    }

    #[test]
    fn test_parse_statement_reassignment() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1));
        let statement =
            Statement::Reassignment("a".into(), Box::new(Expression::Literal(Value::Number(2))));
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2));
    }

    #[test]
    #[should_panic(expected = "could not find identifier a")]
    fn test_parse_statement_reassignment_identifier_not_found() {
        let mut symbols = HashMap::new();
        let statement =
            Statement::Reassignment("a".into(), Box::new(Expression::Literal(Value::Number(2))));
        parse_statement(statement, &mut symbols);
    }

    #[test]
    fn test_parse_statement_function_definintion() {
        let mut symbols = HashMap::new();
        let statement = Statement::FunctionDefinition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Return(Box::new(Expression::Binary(
                    Box::new(Expression::Identifier("a".into())),
                    Operator::MathematicalOperator(MathematicalOperator::Plus),
                    Box::new(Expression::Identifier("b".into())),
                ))),
            )])),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(
            symbols.get("adder").unwrap(),
            &Value::Function(
                vec!["a".into(), "b".into()],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("a".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("b".into()))
                    )))
                ),]))
            )
        );
    }

    #[test]
    fn test_parse_statement_conditional() {
        let mut symbols = HashMap::new();
        let statement = Statement::Conditional(
            Expression::Literal(Value::Boolean(true)),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Assignment("a".into(), Box::new(Expression::Literal(Value::Number(1)))),
            )])),
            Box::new(None),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1));
    }

    #[test]
    fn test_parse_statement_conditional_else() {
        let mut symbols = HashMap::new();
        let statement = Statement::Conditional(
            Expression::Literal(Value::Boolean(false)),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Assignment("a".into(), Box::new(Expression::Literal(Value::Number(1)))),
            )])),
            Box::new(Some(Statement::Block(vec![
                StatementExpression::Statement(Statement::Assignment(
                    "a".into(),
                    Box::new(Expression::Literal(Value::Number(2))),
                )),
            ]))),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2));
    }

    #[test]
    fn test_parse_block() {
        let mut symbols = HashMap::new();
        let block = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "a".into(),
                Box::new(Expression::Literal(Value::Number(1))),
            )),
            StatementExpression::Statement(Statement::Assignment(
                "b".into(),
                Box::new(Expression::Literal(Value::Number(2))),
            )),
            StatementExpression::Statement(Statement::Assignment(
                "c".into(),
                Box::new(Expression::Literal(Value::Number(3))),
            )),
        ]);
        parse_block(Box::new(block), &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1));
        assert_eq!(symbols.get("b").unwrap(), &Value::Number(2));
        assert_eq!(symbols.get("c").unwrap(), &Value::Number(3));
    }

    #[test]
    fn test_parse_function_call() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "adder".into(),
            Value::Function(
                vec!["a".into(), "b".into()],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("a".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("b".into())),
                    ))),
                )])),
            ),
        );
        let expression = Expression::FunctionCall(
            Box::new(Expression::Identifier("adder".into())),
            vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
            ],
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(3));
    }

    #[test]
    fn test_function_call_with_function_expression() {
        let mut symbols = HashMap::new();
        let expression = Expression::FunctionCall(
            Box::new(Expression::Literal(Value::Function(
                vec!["a".into(), "b".into()],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("a".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("b".into())),
                    ))),
                )])),
            ))),
            vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
            ],
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(3));
    }

    #[test]
    #[should_panic(expected = "could not find identifier adder")]
    fn test_parse_function_call_identifier_not_found() {
        let mut symbols = HashMap::new();
        let expression = Expression::FunctionCall(
            Box::new(Expression::Identifier("adder".into())),
            vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
            ],
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Null);
    }

    #[test]
    fn test_parse_collection() {
        let mut symbols = HashMap::new();
        let expression = Expression::Collection(vec![
            Expression::Literal(Value::Number(1)),
            Expression::Literal(Value::Number(2)),
            Expression::Literal(Value::Number(3)),
        ]);
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(
            value,
            Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3),])
        );
    }

    #[test]
    fn test_parse_collection_of_collections() {
        let mut symbols = HashMap::new();
        let expression = Expression::Collection(vec![
            Expression::Collection(vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
                Expression::Literal(Value::Number(3)),
            ]),
            Expression::Collection(vec![
                Expression::Literal(Value::Number(4)),
                Expression::Literal(Value::Number(5)),
                Expression::Literal(Value::Number(6)),
            ]),
            Expression::Collection(vec![
                Expression::Literal(Value::Number(7)),
                Expression::Literal(Value::Number(8)),
                Expression::Literal(Value::Number(9)),
            ]),
        ]);
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(
            value,
            Value::Collection(vec![
                Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3),]),
                Value::Collection(vec![Value::Number(4), Value::Number(5), Value::Number(6),]),
                Value::Collection(vec![Value::Number(7), Value::Number(8), Value::Number(9),]),
            ])
        );
    }

    #[test]
    fn test_parse_for() {
        let mut symbols = HashMap::new();
        symbols.insert("x".into(), Value::Number(0));
        let statement = Statement::For(
            "item".into(),
            Box::new(Expression::Collection(vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
                Expression::Literal(Value::Number(3)),
            ])),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("item".into())),
                    )),
                ),
            )])),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(6));
    }

    #[test]
    fn test_parse_for_with_nested_collection() {
        let mut symbols = HashMap::new();
        symbols.insert("x".into(), Value::Number(0));
        let statement = Statement::For(
            "collection".into(),
            Box::new(Expression::Collection(vec![
                Expression::Collection(vec![
                    Expression::Literal(Value::Number(1)),
                    Expression::Literal(Value::Number(2)),
                    Expression::Literal(Value::Number(3)),
                ]),
                Expression::Collection(vec![
                    Expression::Literal(Value::Number(4)),
                    Expression::Literal(Value::Number(5)),
                    Expression::Literal(Value::Number(6)),
                ]),
                Expression::Collection(vec![
                    Expression::Literal(Value::Number(7)),
                    Expression::Literal(Value::Number(8)),
                    Expression::Literal(Value::Number(9)),
                ]),
            ])),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::For(
                    "item".into(),
                    Box::new(Expression::Identifier("collection".into())),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Identifier("item".into())),
                            )),
                        ),
                    )])),
                ),
            )])),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(45));
    }

    #[test]
    fn test_for_loop_with_break() {
        let mut symbols = HashMap::new();
        symbols.insert("x".into(), Value::Number(0));
        let statement = Statement::For(
            "item".into(),
            Box::new(Expression::Collection(vec![
                Expression::Literal(Value::Number(1)),
                Expression::Literal(Value::Number(2)),
                Expression::Literal(Value::Number(3)),
            ])),
            Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("item".into())),
                    )),
                )),
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
                        Box::new(Expression::Literal(Value::Number(2))),
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Break,
                    )])),
                    Box::new(None),
                )),
            ])),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(3));
    }

    #[test]
    fn test_parse_statement_loop() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(0))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Literal(Value::Number(1))),
                    )),
                )),
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::Equal),
                        Box::new(Expression::Literal(Value::Number(10))),
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Break,
                    )])),
                    Box::new(None),
                )),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(10));
    }

    #[test]
    fn test_parse_statement_loop_with_else() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(3))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::LessThan),
                        Box::new(Expression::Literal(Value::Number(10))),
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1))),
                            )),
                        ),
                    )])),
                    Box::new(Some(Statement::Block(vec![
                        StatementExpression::Statement(Statement::Break),
                    ]))),
                )),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(10));
    }

    #[test]
    fn test_parse_statement_loop_with_else_immediately_breaks() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(15))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::LessThan),
                        Box::new(Expression::Literal(Value::Number(10))),
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1))),
                            )),
                        ),
                    )])),
                    Box::new(Some(Statement::Block(vec![
                        StatementExpression::Statement(Statement::Break),
                    ]))),
                )),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(15));
    }

    #[test]
    fn test_parse_index_accessor() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "x".into(),
            Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3)]),
        );
        let statement = Statement::Reassignment(
            "x".into(),
            Box::new(Expression::Accessor(
                Box::new(Expression::Identifier("x".into())),
                Accessor::Index(Box::new(Expression::Literal(Value::Number(1)))),
            )),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(2));
    }

    #[test]
    fn test_parse_index_accessor_multiple_indices() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "x".into(),
            Value::Collection(vec![
                Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3)]),
                Value::Collection(vec![Value::Number(4), Value::Number(5), Value::Number(6)]),
                Value::Collection(vec![Value::Number(7), Value::Number(8), Value::Number(9)]),
            ]),
        );
        let statement = Statement::Reassignment(
            "x".into(),
            Box::new(Expression::Accessor(
                Box::new(Expression::Accessor(
                    Box::new(Expression::Identifier("x".into())),
                    Accessor::Index(Box::new(Expression::Literal(Value::Number(1)))),
                )),
                Accessor::Index(Box::new(Expression::Literal(Value::Number(1)))),
            )),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(5),);
    }

    #[test]
    #[should_panic(expected = "property accessors are not yet implemented")]
    fn test_parse_property_accessor() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "x".into(),
            Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3)]),
        );
        let statement = Statement::Reassignment(
            "x".into(),
            Box::new(Expression::Accessor(
                Box::new(Expression::Identifier("x".into())),
                Accessor::Property(Value::String("length".into())),
            )),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(3));
    }

    #[test]
    fn test_parse_statement_loop_with_continue() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(3.0))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::LessThan),
                        Box::new(Expression::Literal(Value::Number(10.0))),
                    ),
                    Box::new(Statement::Block(vec![
                        StatementExpression::Statement(Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1.0))),
                            )),
                        )),
                        StatementExpression::Statement(Statement::Continue),
                    ])),
                    Box::new(None),
                )),
                StatementExpression::Statement(Statement::Break),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(10.0));
    }

    #[test]
    fn test_parse_statement_loop_with_early_continue() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(3.0))),
            )),
            StatementExpression::Statement(Statement::Assignment(
                "y".into(),
                Box::new(Expression::Literal(Value::Number(5.0))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("y".into())),
                        Operator::ComparisonOperator(ComparisonOperator::LessThan),
                        Box::new(Expression::Literal(Value::Number(10.0))),
                    ),
                    Box::new(Statement::Block(vec![
                        StatementExpression::Statement(Statement::Reassignment(
                            "y".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("y".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1.0))),
                            )),
                        )),
                        StatementExpression::Statement(Statement::Continue),
                        StatementExpression::Statement(Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1.0))),
                            )),
                        )),
                    ])),
                    Box::new(None),
                )),
                StatementExpression::Statement(Statement::Break),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(3.0));
        assert_eq!(symbols.get("y").unwrap(), &Value::Number(10.0));
    }
}
