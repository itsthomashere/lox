use std::{collections::btree_set::Union, iter::Peekable, vec::IntoIter};

pub mod ast;
pub(crate) mod precedence;
use ast::{
    AssignExpression, BinaryExpression, BinaryOperator, CallExpression, Expression,
    GroupingExpression, Identifier, IndexExpression, LetStatement, ListExpression,
    LogicalExpression, LogicalOperator, PropertyExpression, Statement, UnaryExpression,
    UnaryOperator,
};
use precedence::Precedence;

use crate::{
    lexer::Lexer,
    token::{
        positions::{Diagnostic, Span, WithSpan},
        Token, TokenKind,
    },
};

const EOF_TOKEN: WithSpan<Token> = WithSpan::empty(Token::Eof);

#[derive(Debug, Clone)]
pub struct Parser {
    tokens_iter: Peekable<IntoIter<WithSpan<Token>>>,
    errors: Vec<Diagnostic>,
    cursor: usize,
}

impl Parser {
    pub fn from_code(code: &str) -> Self {
        let errors = Vec::default();
        let tokens_iter = Lexer::new(code).lex_with_context().into_iter().peekable();

        Self {
            tokens_iter,
            errors,
            cursor: 0,
        }
    }

    pub fn from_token_span(token_span: Vec<WithSpan<Token>>) -> Self {
        Self {
            tokens_iter: token_span.into_iter().peekable(),
            errors: Vec::default(),
            cursor: 0,
        }
    }

    pub fn from_lexer(mut lexer: Lexer) -> Self {
        Self {
            tokens_iter: lexer.lex_with_context().into_iter().peekable(),
            errors: Vec::default(),
            cursor: 0,
        }
    }

    fn next(&mut self) -> Option<WithSpan<Token>> {
        match self.tokens_iter.next() {
            Some(token) => {
                self.cursor += 1;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_token(&mut self) -> Option<WithSpan<Token>> {
        // Basically workaround the fact that token doesnt implement COPY
        // So we can't just match without referencing *self twice
        self.tokens_iter.peek().cloned()
    }

    fn peek_kind(&mut self) -> TokenKind {
        match self.peek_token() {
            Some(token) => token.into(),
            None => EOF_TOKEN.into(),
        }
    }

    fn check_next_kind(&mut self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    fn errors(&mut self, message: &str, span: Span) {
        self.errors.push(Diagnostic {
            span,
            message: message.to_string(),
        })
    }

    fn parse_program(&mut self) -> Vec<WithSpan<Statement>> {
        let mut statements: Vec<WithSpan<Statement>> = Vec::default();
        while let Some(token) = self.peek_token() {
            let statement = self.parse_statement(token);
            if statement.is_none() {
                continue;
            }

            statements.push(statement.unwrap())
        }
        statements
    }

    fn parse_statement(&mut self, token: WithSpan<Token>) -> Option<WithSpan<Statement>> {
        match Into::<TokenKind>::into(token) {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => todo!(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<WithSpan<Statement>> {
        let begin_span = self.expect(TokenKind::Let).ok()?.span;
        let identifier = self.expect(TokenKind::Identifier).ok()?;
        let value = match identifier.val {
            Token::Identifier(ident) => ident,
            _ => panic!("It should be an identifier, you fucked up"),
        };

        let identifier = Identifier {
            val: value,
            span: identifier.span,
        };

        self.expect(TokenKind::Assign).ok()?;
        let initializer = self.parse_expression(Precedence::None);
        let let_statement = LetStatement {
            identifier,
            initializer,
        };

        let end_span = self.expect(TokenKind::Semicolon).ok()?.span;

        Some(WithSpan::new(
            Statement::Let(let_statement),
            Span::union_span(begin_span, end_span),
        ))
    }

    fn parse_expresion_statement(&mut self) -> Option<WithSpan<Statement>> {
        todo!()
    }

    fn parse_return_statement(&mut self) -> Option<WithSpan<Statement>> {
        todo!()
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<WithSpan<Expression>> {
        let mut left_expr = self.parse_prefix()?;
        while self.peek_kind() != TokenKind::Semicolon
            && precedence < Precedence::from(self.peek_kind())
        {
            left_expr = self.parse_infix(left_expr)?;
        }

        Some(left_expr)
    }

    fn parse_prefix(&mut self) -> Option<WithSpan<Expression>> {
        match self.peek_kind() {
            TokenKind::This
            | TokenKind::Super
            | TokenKind::Null
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Identifier
            | TokenKind::String
            | TokenKind::Number => self.parse_primary(),
            TokenKind::Bang | TokenKind::Minus => self.parse_unary(), // parse unary
            TokenKind::LParen => self.parse_grouping(),               // parse grouping
            TokenKind::LBracket => self.parse_list(),                 // parse list
            _ => {
                let peek = self.peek_token().unwrap_or(EOF_TOKEN);
                self.errors(&format!("Unexpected {}", peek.val), peek.span);
                None
            }
        }
    }

    fn parse_infix(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        match self.peek_kind() {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Asterisk
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual
            | TokenKind::Slash
            | TokenKind::Greater => self.parse_binary(left), // parse binary
            TokenKind::Or | TokenKind::And => self.parse_logical(left), // parse logical
            TokenKind::Assign => self.parse_assign(left),               // parse assign
            TokenKind::LParen => self.parse_call(left),                 // parse call
            TokenKind::LBracket => self.parse_index(left),              // parse index
            TokenKind::Dot => todo!(),                                  // parse parse property
            _ => todo!(),
        }
    }

    fn parse_primary(&mut self) -> Option<WithSpan<Expression>> {
        let token = self
            .next()
            .expect("This should only be called if the next token exists, you fucked up");

        match token.val {
            Token::Null => Some(WithSpan::new(Expression::Nil, token.span)),
            Token::This => Some(WithSpan::new(Expression::This, token.span)),
            Token::Number(n) => Some(WithSpan::new(Expression::Number(n), token.span)),
            Token::String(s) => Some(WithSpan::new(Expression::String(s.clone()), token.span)),
            Token::Identifier(i) => Some(WithSpan::new(Expression::Identifier(i), token.span)),
            Token::True => Some(WithSpan::new(Expression::Boolean(true), token.span)),
            Token::False => Some(WithSpan::new(Expression::Boolean(false), token.span)),
            Token::Super => todo!(),
            _ => {
                self.errors(&format!("Expected primary, got {}", token.val), token.span);
                None
            }
        }
    }

    fn parse_unary(&mut self) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_grouping(&mut self) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_list(&mut self) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_index(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_binary(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_logical(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_assign(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_call(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn parse_property(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        todo!()
    }

    fn expect(&mut self, kind: TokenKind) -> Result<WithSpan<Token>, ()> {
        let next_token = self.next().unwrap_or(EOF_TOKEN);
        if Into::<TokenKind>::into(next_token.clone()) == kind {
            Ok(next_token)
        } else {
            self.errors(
                &format!(
                    "Expected {}, got {}",
                    kind,
                    Into::<TokenKind>::into(next_token.clone())
                ),
                next_token.span,
            );
            Err(())
        }
    }
}
