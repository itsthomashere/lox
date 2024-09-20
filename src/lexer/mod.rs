use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::token::{
    positions::{BytePosition, Span, WithSpan},
    Token,
};

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
            ("true", Token::True),
            ("false", Token::False),
        ]);
        Self {
            char_iter: code.chars().peekable(),
            position: BytePosition::default(),
            keywords,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(&ch) = self.peek() {
            let token = self.match_next_token(ch);
            if token.is_none() {
                continue;
            }
            tokens.push(token.unwrap());
        }
        tokens.push(Token::Eof);
        tokens
    }

    pub fn lex_with_context(&mut self) -> Vec<WithSpan<Token>> {
        let mut tokens: Vec<WithSpan<Token>> = Vec::default();

        while let Some(&ch) = self.peek() {
            let start = self.position.clone();
            let next_token = self.match_next_token(ch);
            if next_token.is_none() {
                continue;
            }

            tokens.push(WithSpan::new(
                next_token.unwrap(),
                Span::new(start.0, self.position.0),
            ))
        }

        tokens.push(WithSpan::new(
            Token::Eof,
            Span::new(self.position.0, self.position.0),
        ));

        tokens
    }

    fn match_next_token(&mut self, ch: char) -> Option<Token> {
        match ch {
            '\n' => {
                self.next().unwrap();
                None
            }
            '\r' => {
                self.next().unwrap();
                None
            }
            '\t' => {
                self.next().unwrap();
                None
            }
            '\0' => {
                self.next().unwrap();
                None
            }
            ' ' => {
                self.next().unwrap();
                None
            }
            ':' => {
                self.next().unwrap();
                Some(Token::Colon)
            }
            ';' => {
                self.next().unwrap();
                Some(Token::Semicolon)
            }
            '+' => {
                self.next().unwrap();
                Some(Token::Plus)
            }
            '-' => {
                self.next().unwrap();
                Some(Token::Minus)
            }
            '*' => {
                self.next();
                Some(Token::Asterisk)
            }
            '/' => {
                self.next().unwrap();
                Some(Token::Slash)
            }
            '(' => {
                self.next().unwrap();
                Some(Token::LParen)
            }
            ')' => {
                self.next().unwrap();
                Some(Token::RParen)
            }
            '{' => {
                self.next().unwrap();
                Some(Token::LBrace)
            }
            '}' => {
                self.next().unwrap();
                Some(Token::RBrace)
            }
            '[' => {
                self.next().unwrap();
                Some(Token::LBracket)
            }
            ']' => {
                self.next().unwrap();
                Some(Token::RBracket)
            }
            '.' => {
                self.next().unwrap();
                Some(Token::Dot)
            }
            ',' => {
                self.next().unwrap();
                Some(Token::Comma)
            }
            '!' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '=') {
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Bang)
                }
            }
            '=' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '=') {
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }
            '<' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '=') {
                    Some(Token::LessEqual)
                } else {
                    Some(Token::Less)
                }
            }
            '>' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '=') {
                    Some(Token::GreaterEqual)
                } else {
                    Some(Token::Greater)
                }
            }
            '&' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '&') {
                    Some(Token::And)
                } else {
                    Some(Token::Unknown('&'))
                }
            }
            '|' => {
                self.next().unwrap();
                if self.consume_if(|c| c == '|') {
                    Some(Token::Or)
                } else {
                    Some(Token::Unknown('|'))
                }
            }
            '"' => Some(self.read_string()),
            _ => {
                if ch.is_ascii_digit() {
                    Some(self.read_number())
                } else {
                    Some(self.read_identifier())
                }
            }
        }
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
        self.next(); // consume the opening '"'
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
        while let Some(&ch) = self.peek() {
            if f(ch) {
                self.next().unwrap();
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
                self.next().unwrap();
                return true;
            } else {
                return false;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {

    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn test_let_lexing() {
        let input = r#"
            let x = 10.5;
            let y = 10;
        "#;
        let expected = vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Number(10.5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("y".to_string()),
            Token::Assign,
            Token::Number(10.0),
            Token::Semicolon,
            Token::Eof,
        ];

        let result = Lexer::new(input).lex();
        assert_eq!(result.len(), expected.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected[i])
        }
    }

    #[test]
    fn test_keywords() {
        let input = r#"
            class Thomas;
            if;
            else;
            for;
            while;
            super;
            this;
            let;
            extends;
            nil;
            return;
            fun;
        "#;
        let expected = [
            Token::Class,
            Token::Identifier("Thomas".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Semicolon,
            Token::Else,
            Token::Semicolon,
            Token::For,
            Token::Semicolon,
            Token::While,
            Token::Semicolon,
            Token::Super,
            Token::Semicolon,
            Token::This,
            Token::Semicolon,
            Token::Let,
            Token::Semicolon,
            Token::Extends,
            Token::Semicolon,
            Token::Null,
            Token::Semicolon,
            Token::Return,
            Token::Semicolon,
            Token::Function,
            Token::Semicolon,
            Token::Eof,
        ];
        let result = Lexer::new(input).lex();
        assert_eq!(
            result.len(),
            expected.len(),
            "Expected len to be{}, got: {}",
            expected.len(),
            result.len()
        );

        for i in 0..result.len() {
            assert_eq!(
                expected[i], result[i],
                "Expected token to be: {}, got: {}",
                expected[i], result[i]
            );
        }
    }

    #[test]
    fn test_delimiters() {
        let input = r#"
            (){}[].;=:+- */<>!
        "#;
        let result = Lexer::new(input).lex();
        let expected: Vec<Token> = vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Dot,
            Token::Semicolon,
            Token::Assign,
            Token::Colon,
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Less,
            Token::Greater,
            Token::Bang,
            Token::Eof,
        ];

        assert_eq!(
            result.len(),
            expected.len(),
            "Expected len to be{}, got: {}",
            expected.len(),
            result.len()
        );

        for i in 0..result.len() {
            assert_eq!(
                expected[i], result[i],
                "Expected token to be: {}, got: {}",
                expected[i], result[i]
            );
        }
    }

    #[test]
    fn test_string() {
        let code = r#"
        "thomas"
        "thomas01"
        "100502"
        "thomas
        "#;
        let result = Lexer::new(code).lex();
        let expected = [
            Token::String("thomas".to_string()),
            Token::String("thomas01".to_string()),
            Token::String("100502".to_string()),
            Token::NonClosingString,
            Token::Eof,
        ];
        eprint!("{:?}", result);

        assert_eq!(
            result.len(),
            expected.len(),
            "Expected len to be{}, got: {}",
            expected.len(),
            result.len()
        );

        for i in 0..result.len() {
            assert_eq!(
                expected[i], result[i],
                "Expected token to be: {}, got: {}",
                expected[i], result[i]
            );
        }
    }

    #[test]
    fn test_multiple_character_operation() {
        let code = r#"
            ==
            != 
            <=
            >=
            &&
            ||
            &
            |
        "#;
        let result = Lexer::new(code).lex();
        let expected = [
            Token::Equal,
            Token::NotEqual,
            Token::LessEqual,
            Token::GreaterEqual,
            Token::And,
            Token::Or,
            Token::Unknown('&'),
            Token::Unknown('|'),
            Token::Eof,
        ];

        assert_eq!(
            result.len(),
            expected.len(),
            "Expected len to be{}, got: {}",
            expected.len(),
            result.len()
        );

        for i in 0..result.len() {
            assert_eq!(
                expected[i], result[i],
                "Expected token to be: {}, got: {}",
                expected[i], result[i]
            );
        }
    }

    #[test]
    fn test_boolean_types() {
        let code = r#"
            true;
            false;
            ,;
        "#;
        let result = Lexer::new(code).lex();
        let expected = [
            Token::True,
            Token::Semicolon,
            Token::False,
            Token::Semicolon,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(
            result.len(),
            expected.len(),
            "Expected len to be{}, got: {}",
            expected.len(),
            result.len()
        );

        for i in 0..result.len() {
            assert_eq!(
                expected[i], result[i],
                "Expected token to be: {}, got: {}",
                expected[i], result[i]
            );
        }
    }
}
