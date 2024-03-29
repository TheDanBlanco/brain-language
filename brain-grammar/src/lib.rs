use brain_token::stream::TokenStream;

use self::grammar::{context::Context, output::Output, token::BrainToken, Nodes, Parse, Resolve};

pub mod grammar;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stream: TokenStream<BrainToken>,
    pub context: Context,
    verbose: bool,
}

impl Program {
    pub fn new(stream: TokenStream<BrainToken>, verbose: bool) -> Self {
        if verbose {
            println!("Stream: {:#?}", stream);
        }

        Program {
            stream,
            verbose,
            context: Context::new(),
        }
    }

    pub fn run(&mut self) -> Result<Output, Box<dyn std::error::Error>> {
        let nodes = Nodes::parse(&mut self.stream.clone())?;

        if self.verbose {
            println!("Nodes: {:#?}", nodes);
        }

        nodes.resolve(&mut self.context)
    }
}
