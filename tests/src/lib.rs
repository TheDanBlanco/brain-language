mod attribute;

#[cfg(test)]
mod tests {
    // brain does NOT support unary expressions, bitwise operations.

    use std::collections::BTreeMap;

    use brain_grammar::{
        grammar::{token::BrainToken, value::Value},
        Program,
    };
    use brain_token::Brain;

    fn run(source: String) -> Result<Program, Box<dyn std::error::Error>> {
        let stream = BrainToken::lex(source);
        let mut program = Program::new(stream.unwrap(), false);
        program.run()?;
        Ok(program)
    }

    #[test]
    fn test_with_verbose() {
        let source = "let first = 0".to_string();

        let stream = BrainToken::lex(source);
        let mut program = Program::new(stream.unwrap(), true);

        let _ = program.run();

        assert_eq!(
            program.context.symbols.get("first"),
            Some(&Value::Number(0))
        );
    }

    #[test]
    fn assignment() {
        let source = "let first = 0".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::Number(0))
        );
    }

    #[test]
    fn assignment_with_binary_expression_mathematical_numbers() {
        let source = "let first = 0 + 1".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn assignment_with_binary_expression_mathematical_strings() {
        let source = r#"let first = "a" + "b""#.to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::String("ab".to_string()))
        );
    }

    #[test]
    fn assignment_with_binary_expression_comparison_numbers() {
        let source = "let first = 0 == 1".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::Boolean(false))
        );
    }

    #[test]
    fn function_definition_binary_return() {
        let source = "fn add(a, b) { return a + b }; let x = add(1, 2)".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(3))
        );
    }

    #[test]
    fn map() {
        let source = "let x = { a: 1, b: 2 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Map(BTreeMap::from_iter(vec![
                (Value::String("a".to_string()), Value::Number(1)),
                (Value::String("b".to_string()), Value::Number(2))
            ])))
        );
    }

    #[test]
    fn collection() {
        let source = "let x = [1, 2, 3]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Collection(vec![
                Value::Number(1),
                Value::Number(2),
                Value::Number(3)
            ]))
        );
    }

    #[test]
    fn collection_of_collections() {
        let source = "let x = [[1, 2], [3, 4]]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Collection(vec![
                Value::Collection(vec![Value::Number(1), Value::Number(2)]),
                Value::Collection(vec![Value::Number(3), Value::Number(4)])
            ]))
        );
    }

    #[test]
    fn collection_with_map() {
        let source = "let x = [{ a: 1, b: 2 }, { a: 3, b: 4 }]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Collection(vec![
                Value::Map(BTreeMap::from_iter(vec![
                    (Value::String("a".to_string()), Value::Number(1)),
                    (Value::String("b".to_string()), Value::Number(2))
                ])),
                Value::Map(BTreeMap::from_iter(vec![
                    (Value::String("a".to_string()), Value::Number(3)),
                    (Value::String("b".to_string()), Value::Number(4))
                ]))
            ]))
        );
    }

    #[test]
    fn collection_with_index_accessor() {
        let source = "let x = [1, 2, 3]; let y = x[0]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn map_with_field_accessor() {
        let source = "let x = { a: 1, b: 2 }; let y = x.a".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn map_with_field_accessor_with_index_accessor() {
        let source = "let x = [{ a: 1, b: 2 }, { a: 3, b: 4 }]; let y = x[0].a".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn r#loop() {
        let source = "let x = 0; loop { if x < 10 { x = x + 1; continue; } break; }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(10))
        );
    }

    #[test]
    fn r#if() {
        let source = "let x = 0; if x == 0 { x = 1 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn r#if_else() {
        let source = "let x = 0; if x == 1 { x = 1 } else { x = 2 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(2))
        );
    }

    #[test]
    fn r#for() {
        let source = "let x = 0; for i in [1, 2, 3] { x = x + i }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(6))
        );
    }

    #[test]
    fn r#for_with_break() {
        let source = "let x = 0; for i in [1, 2, 3] { x = x + i; if x == 3 { break } }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(3))
        );
    }

    #[test]
    fn r#bitwise_and() {
        let source = "let x = 5 & 1;".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(1))
        );
    }

    #[test]
    fn r#bitwise_or() {
        let source = "let x = 5 | 1;".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::Number(5))
        );
    }

    #[test]
    fn print() {
        let source = "print(1)".to_string();

        let result = run(source);

        assert!(result.is_ok())
    }
}
