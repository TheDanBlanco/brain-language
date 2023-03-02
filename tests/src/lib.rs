use brain_attribute::Brain;

#[derive(Brain)]
enum Token {
    #[token("let")]
    Let,

    #[token("assign")]
    Assign,

    #[regex(["a-zA-z"])]
    Identifier(String),
}