use brain_token::stream::TokenStream;

pub struct Lexer {
    input: String,
    current_position: usize,
    next_position: usize,
    stream: TokenStream,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input,
            current_position: 0,
            next_position: 0,
            stream: TokenStream::new(),
        }
    }
}
