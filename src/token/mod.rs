pub enum TokenType {
    // Identifier
    Identifier,
    String,
    Number,
    // Keywords
    Function,
    Let,
    Return,
    If,
    Else,
    While,
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Colon,
    Dot,
    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Assign,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    // EOF and Errors
    Eof,
    Illegal,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
}
