use std::{env, fs::File, io::{self, BufRead}, path::Path};
use crate::lang::{lexer::Lexer, parser::Parser};

mod lang;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];

    if let Ok(lines) = read_lines(file) {
        for line in lines {
            if let Ok(line) = line {
                if line != "" {
                    let mut lexer = Lexer::new(line);
                    let tokens = lexer.get_all_tokens();
                    let mut parser = Parser::new(tokens);
                    let ast = parser.parse();
                    println!("{:#?}", ast);
                }

                // print all tokens
                // for token in tokens {
                //     println!("{:?}", token);
                // }
            }

            println!("\n");
        }
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
