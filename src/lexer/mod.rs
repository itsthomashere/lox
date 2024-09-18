use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::token::Token;

pub struct Lexer<'a> {
    char_iter: Peekable<Chars<'a>>,
    position: BytePosition,
    keywords: HashMap<&'static str, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        let keywords: HashMap<&'static str, Token> = HashMap::from([
            ("let", Token::Let),
            ("class", Token::Class),
            ("this", Token::This),
            ("extends", Token::Extends),
            ("super", Token::Super),
            ("for", Token::For),
            ("else", Token::Else),
            ("nil", Token::Null),
            ("return", Token::Return),
            ("while", Token::While),
            ("fun", Token::Function),
            ("if", Token::If),
        ]);
        Self {
            char_iter: code.chars().peekable(),
            position: BytePosition::default(),
            keywords,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(ch) = self.peek() {
            let token = match ch {
                '\n' => {
                    self.next().unwrap();
                    continue;
                }
                '\r' => {
                    self.next().unwrap();
                    continue;
                }
                '\t' => {
                    self.next().unwrap();
                    continue;
                }
                '\0' => {
                    self.next().unwrap();
                    continue;
                }
                ' ' => {
                    self.next().unwrap();
                    continue;
                }
                ':' => {
                    self.next().unwrap();
                    Token::Colon
                }
                ';' => {
                    self.next().unwrap();
                    Token::Semicolon
                }
                '+' => {
                    self.next().unwrap();
                    Token::Plus
                }
                '-' => {
                    self.next().unwrap();
                    Token::Minus
                }
                '*' => {
                    self.next();
                    Token::Asterisk
                }
                '/' => {
                    self.next().unwrap();
                    Token::Slash
                }
                '(' => {
                    self.next().unwrap();
                    Token::LParen
                }
                ')' => {
                    self.next().unwrap();
                    Token::RParen
                }
                '{' => {
                    self.next().unwrap();
                    Token::LBrace
                }
                '}' => {
                    self.next().unwrap();
                    Token::RBrace
                }
                '[' => {
                    self.next().unwrap();
                    Token::LBracket
                }
                ']' => {
                    self.next().unwrap();
                    Token::RBracket
                }
                '.' => {
                    self.next().unwrap();
                    Token::Dot
                }
                '!' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '=') {
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                '=' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '=') {
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                '<' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '=') {
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                '>' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '=') {
                        Token::Greater
                    } else {
                        Token::GreaterEqual
                    }
                }
                '&' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '&') {
                        Token::And
                    } else {
                        Token::Unknown('&')
                    }
                }
                '|' => {
                    self.next().unwrap();
                    if self.consume_if(|c| c == '|') {
                        Token::Or
                    } else {
                        Token::Unknown('|')
                    }
                }
                '"' => self.read_string(),
                _ => {
                    if ch.is_ascii_digit() {
                        self.read_number()
                    } else {
                        self.read_identifier()
                    }
                }
            };
            tokens.push(token);
        }
        tokens.push(Token::Eof);
        tokens
    }

    fn read_number(&mut self) -> Token {
        let number: String = self
            .consume_while(|c| c.is_ascii_digit() || c == '.')
            .into_iter()
            .collect();

        Token::Number(number.parse::<f64>().unwrap())
    }
    fn read_identifier(&mut self) -> Token {
        let identifier: String = self
            .consume_while(|f| f.is_ascii_alphanumeric())
            .into_iter()
            .collect();
        if let Some(keyword) = self.keywords.get(identifier.as_str()) {
            return keyword.clone();
        }
        Token::Identifier(identifier)
    }
    fn read_string(&mut self) -> Token {
        let string: String = self.consume_while(|c| c != '"').into_iter().collect();
        match self.next() {
            Some(_) => Token::String(string),
            None => Token::NonClosingString,
        }
    }

    fn next(&mut self) -> Option<char> {
        let next = self.char_iter.next();
        if let Some(c) = next {
            self.position.shift(c);
        }

        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.char_iter.peek()
    }

    fn consume_while<F>(&mut self, f: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars: Vec<char> = Vec::default();
        while let Some(&ch) = self.char_iter.peek() {
            if f(ch) {
                self.char_iter.next().unwrap();
                chars.push(ch)
            } else {
                break;
            }
        }

        chars
    }

    fn consume_if<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(&ch) = self.char_iter.peek() {
            if f(ch) {
                self.char_iter.next().unwrap();
                return true;
            } else {
                return false;
            }
        }

        false
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct BytePosition(pub u32);

impl BytePosition {
    fn shift(&mut self, ch: char) {
        self.0 += ch.len_utf8() as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_lexing() {
        let input = r#"
            let x = 10.5;
            let y = 10;
        "#;
    }
}
