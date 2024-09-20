pub mod positions;

use std::fmt::Display;

use positions::WithSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Reserverd keyword
    This,     // this
    Super,    // super
    For,      // for
    While,    // while
    If,       // if
    Else,     // else
    Class,    // class
    Function, // fun
    Extends,  // extends
    Null,     // nil
    Return,   // return
    Let,      // let
    True,     // true
    False,    // false
    // Delimiters
    LParen,       // (
    RParen,       // )
    LBrace,       // {
    RBrace,       // }
    LBracket,     // [
    RBracket,     // ]
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    Equal,        // ==
    NotEqual,     // !=
    Assign,       // =
    And,          // &&
    Or,           // ||
    Bang,         // !
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    Comma,        // ,
    //Literals
    String,
    Number,
    Identifier,
    // Others
    Eof,
    Unknown,
    NonClosingString,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Reserverd keyword
    This,     // this
    Super,    // super
    For,      // for
    While,    // while
    If,       // if
    Else,     // else
    Class,    // class
    Function, // fun
    Extends,  // extends
    Null,     // nil
    Return,   // return
    Let,      // let
    True,     // true
    False,    // false
    // Delimiters
    LParen,       // (
    RParen,       // )
    LBrace,       // {
    RBrace,       // }
    LBracket,     // [
    RBracket,     // ]
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    Equal,        // ==
    NotEqual,     // !=
    Assign,       // =
    And,          // &&
    Or,           // ||
    Bang,         // !
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    Comma,        // ,
    //Literals
    String(String),
    Number(f64),
    Identifier(String),
    // Others
    Eof,
    Unknown(char),
    NonClosingString,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind: TokenKind = self.into();
        write!(f, "{}", kind)
    }
}

impl From<&Token> for TokenKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::This => TokenKind::This,
            Token::Super => TokenKind::Super,
            Token::For => TokenKind::For,
            Token::While => TokenKind::While,
            Token::If => TokenKind::If,
            Token::Else => TokenKind::Else,
            Token::Class => TokenKind::Class,
            Token::Function => TokenKind::Function,
            Token::Extends => TokenKind::Extends,
            Token::Null => TokenKind::Null,
            Token::Return => TokenKind::Return,
            Token::Let => TokenKind::Let,
            Token::LParen => TokenKind::LParen,
            Token::RParen => TokenKind::RParen,
            Token::LBrace => TokenKind::LBrace,
            Token::RBrace => TokenKind::RBrace,
            Token::LBracket => TokenKind::LBracket,
            Token::RBracket => TokenKind::RBracket,
            Token::Plus => TokenKind::Plus,
            Token::Minus => TokenKind::Minus,
            Token::Asterisk => TokenKind::Asterisk,
            Token::Slash => TokenKind::Slash,
            Token::Less => TokenKind::Less,
            Token::Greater => TokenKind::Greater,
            Token::LessEqual => TokenKind::LessEqual,
            Token::GreaterEqual => TokenKind::GreaterEqual,
            Token::Equal => TokenKind::Equal,
            Token::NotEqual => TokenKind::NotEqual,
            Token::Assign => TokenKind::Assign,
            Token::And => TokenKind::And,
            Token::Or => TokenKind::Or,
            Token::Bang => TokenKind::Bang,
            Token::String(_) => TokenKind::String,
            Token::Number(_) => TokenKind::Number,
            Token::Identifier(_) => TokenKind::Identifier,
            Token::Eof => TokenKind::Eof,
            Token::Unknown(_) => TokenKind::Unknown,
            Token::NonClosingString => TokenKind::NonClosingString,
            Token::Dot => TokenKind::Dot,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Colon => TokenKind::Colon,
            Token::True => TokenKind::True,
            Token::False => TokenKind::False,
            Token::Comma => TokenKind::Comma,
        }
    }
}
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::This => write!(f, "this"),
            TokenKind::Super => write!(f, "super"),
            TokenKind::For => write!(f, "for"),
            TokenKind::While => write!(f, "while"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Class => write!(f, "class"),
            TokenKind::Function => write!(f, "fun"),
            TokenKind::Extends => write!(f, "extends"),
            TokenKind::Null => write!(f, "nil"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Assign => write!(f, "="),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Eof => write!(f, "<eof>"),
            TokenKind::Unknown => write!(f, "<unknown>"),
            TokenKind::NonClosingString => write!(f, "<nonclosingstring>"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Comma => write!(f, ","),
        }
    }
}

impl From<WithSpan<Token>> for TokenKind {
    fn from(value: WithSpan<Token>) -> Self {
        let token = &value.val;
        token.into()
    }
}

impl From<&WithSpan<Token>> for TokenKind {
    fn from(value: &WithSpan<Token>) -> Self {
        let token = &value.val;
        token.into()
    }
}
