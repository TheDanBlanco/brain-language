use std::{collections::HashMap};

use super::parser::{Operator, Value, Expression, Statement, StatementExpression};

fn parse(expression: Expression, symbols: &mut HashMap<String, Value>) -> Value {
    match expression {
        Expression::Literal(value) => {
            return match value {
                Value::Number(num) => Value::Number(num),
                Value::String(str) => Value::String(str),
                _ => Value::Null,
            }
        }
        Expression::Binary(lhs, operator, rhs) => {
            let lhs = parse(*lhs, symbols);
            let rhs = parse(*rhs, symbols);

            return match (lhs, rhs, operator) {
                (Value::Number(lhs), Value::Number(rhs), Operator::Plus) => Value::Number(lhs + rhs),
                (Value::String(lhs), Value::String(rhs), Operator::Plus) => Value::String(format!("{}{}", lhs, rhs)),
                _ => {
                    panic!("unable to parse binary expression")
                }
            }
        }
        Expression::FunctionCall(identifier, arguments) => {
            if identifier == "print" {
                let arg = arguments[0].clone();
                let val = parse(arg, symbols);
                println!("{:#?}", val)
            }
        }
        Expression::Identifier(identifier) => {
            if let Some(val) = symbols.get(&identifier) {
                return Value::to_owned(val);
            }

            panic!("could not find identifier in symbols table");
        }
        _ => println!("couldn't parse expression")
    }

    Value::Null
}

pub fn evaluate(statement_expression: StatementExpression, symbols: &mut HashMap<String, Value>) {
    match statement_expression {
        StatementExpression::Expression(expression) => {
            parse(expression, symbols);
        },
        StatementExpression::Statement(statement) => {
            match statement {
                Statement::Assignment(identifier, expression) => {
                    let value = parse(*expression, symbols);
                    symbols.insert(identifier, value);
                }
                _ => println!("couldnt figure out node: {:#?}", statement)
   
            }
        }
    }
}