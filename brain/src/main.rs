use brain_grammar::{grammar::token::BrainToken, Program};
use brain_token::Brain;
use cli::Cli;
use io::read_file;

mod cli;
mod io;

pub const BRAIN_VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut cli = Cli::new();
    if let Err(err) = cli.parse() {
        println!("{}", err);
        return Ok(());
    }

    if cli.help {
        println!("Usage: brain [file] [--debug] [--help] --version]");
        return Ok(());
    }

    if cli.version {
        println!("brain {BRAIN_VERSION}");
        return Ok(());
    }

    let source = read_file(cli.file).unwrap();

    let stream = BrainToken::lex(source);
    let mut program = Program::new(stream.unwrap(), cli.debug);
    if let Err(err) = program.run() {
        println!("{}", err);
    }

    Ok(())
}
