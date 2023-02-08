use std::{env, fs::File, io::{self, BufRead}, path::Path, collections::HashMap};
use lang::parser::Value;

use crate::lang::{lexer::Lexer, parser::Parser, compiler::run};

mod lang;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let mut symbols: HashMap<String, Value> = HashMap::new();
    let mut input = Vec::new();

    if let Ok(lines) = read_lines(file) {
        for line in lines {
            if let Ok(line) = line {
                if line != "" {
                    input.push(line);            
                }
            }
        }

        let mut lexer = Lexer::new(input.concat());
        let tokens = lexer.get_all_tokens();
        // println!("{:#?}", tokens);
        let mut parser = Parser::new(tokens);
        let ast = parser.create_ast();
        run(ast, &mut symbols);
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
// 