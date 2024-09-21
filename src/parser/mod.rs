use core::panic;
use std::{iter::Peekable, vec::IntoIter};

pub mod ast;
pub(crate) mod precedence;
use ast::{
    AssignExpression, BinaryExpression, BinaryOperator, CallExpression, Expression,
    ExtendsExpression, GroupingExpression, IndexExpression, LetStatement, ListExpression,
    LogicalExpression, LogicalOperator, PropertyExpression, Statement, SuperExpression,
    UnaryExpression, UnaryOperator,
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

    fn check_next_kind<F>(&mut self, f: F) -> bool
    where
        F: Fn(TokenKind) -> bool,
    {
        f(self.peek_kind())
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

    // statement parser

    fn parse_let_statement(&mut self) -> Option<WithSpan<Statement>> {
        let begin_span = self.consume_next_if(TokenKind::Let)?.span;
        let identifier = self.consume_next_if(TokenKind::Identifier)?;
        let value = match identifier.val {
            Token::Identifier(ident) => ident,
            _ => panic!("It should be an identifier, you fucked up"),
        };

        let identifier: WithSpan<Expression> =
            WithSpan::new(Expression::Identifier(value), identifier.span);

        if self.check_next_kind(|kind| kind == TokenKind::Assign) {
            self.next().unwrap();
            self.consume_next_if(TokenKind::Semicolon)?;
        }

        let initializer = self.parse_expression(Precedence::None).map(Box::new);

        let let_statement = LetStatement {
            identifier,
            initializer,
        };

        let end_span = self.consume_next_if(TokenKind::Semicolon)?.span;

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
            match self.parse_infix(left_expr.clone()) {
                Some(expression) => left_expr = expression,
                None => return Some(left_expr),
            };
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
            TokenKind::Extends => self.parse_extends(),
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
            TokenKind::Dot => self.parse_property(left),                // parse parse property
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
            Token::Super => self.parse_super(&token),
            _ => {
                self.errors(&format!("Expected primary, got {}", token.val), token.span);
                None
            }
        }
    }

    // Prefix parsing functions

    fn parse_unary(&mut self) -> Option<WithSpan<Expression>> {
        let operator_token = &self
            .next()
            .expect("Should only be called when next token is unary operator token");
        let expression = match self.parse_expression(Precedence::Unary) {
            Some(expr) => expr,
            None => {
                self.errors("Expected Expr, got Unknown", operator_token.span.clone());
                return None;
            }
        };

        let operator: Box<WithSpan<UnaryOperator>> = Box::new(WithSpan::new(
            Into::<TokenKind>::into(operator_token)
                .try_into()
                .unwrap_or_else(|e| panic!("{}", e)),
            operator_token.span.clone(),
        ));

        let span = Span::union(operator_token, &expression);
        let unary = UnaryExpression {
            operator,
            expression: Box::new(expression),
        };

        Some(WithSpan::new(Expression::Unary(unary), span))
    }

    fn parse_grouping(&mut self) -> Option<WithSpan<Expression>> {
        let start_token = &self
            .next()
            .expect("Should only be called when the next token us left parenthesis");
        let expression = self.parse_expression(Precedence::None)?;
        let right_paren = self.consume_next_if(TokenKind::RParen)?;
        let span = Span::union(start_token, &right_paren);

        let grouping = GroupingExpression {
            expression: Box::new(expression),
        };

        Some(WithSpan::new(Expression::Grouping(grouping), span))
    }

    fn parse_list(&mut self) -> Option<WithSpan<Expression>> {
        let start_token = &self
            .next()
            .expect("Should only be called when the next token us left parenthesis");
        let elements = self.parse_list_items(TokenKind::RBracket);
        let right_bracket = self.consume_next_if(TokenKind::RBracket)?;

        let span = Span::union(start_token, &right_bracket);

        Some(WithSpan::new(
            Expression::List(ListExpression { elements }),
            span,
        ))
    }

    fn parse_list_items(&mut self, closing_kind: TokenKind) -> Vec<WithSpan<Expression>> {
        let mut elements = Vec::new();

        if self.check_next_kind(|f| f != closing_kind) {
            let expr = self.parse_expression(Precedence::None);
            if let Some(expr) = expr {
                elements.push(expr)
            }

            while self.check_next_kind(|f| f == TokenKind::Comma) {
                self.consume_next_if(TokenKind::Comma).unwrap();
                if let Some(expr) = self.parse_expression(Precedence::None) {
                    elements.push(expr)
                }
            }
        }

        elements
    }

    fn parse_super(&mut self, keyword: &WithSpan<Token>) -> Option<WithSpan<Expression>> {
        self.consume_next_if(TokenKind::Dot)?;
        let identifier = Box::new(self.expect_ident()?);
        let span = Span::union(keyword, &identifier);
        Some(WithSpan::new(
            Expression::Super(SuperExpression { identifier }),
            span,
        ))
    }

    fn parse_extends(&mut self) -> Option<WithSpan<Expression>> {
        let extends_token = self
            .next()
            .expect("Should only be called if the next token is extends");
        let identifier = self.expect_ident()?;
        let span = Span::union(&extends_token, &identifier);
        let extends = ExtendsExpression {
            child: Box::new(identifier),
        };

        Some(WithSpan::new(Expression::Extends(extends), span))
    }

    // infix parsing function

    fn parse_index(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        self.next().expect("This should be a token, you fucked up");
        let index = self.parse_expression(Precedence::None)?;

        let right_bracket = self.consume_next_if(TokenKind::RBracket)?;
        let span = Span::union(&left, &right_bracket);

        Some(WithSpan::new(
            Expression::Index(IndexExpression {
                left: Box::new(left),
                index: Box::new(index),
            }),
            span,
        ))
    }

    fn parse_binary(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        let precedence: Precedence = Precedence::from(self.peek_kind());
        let next_token = &self.next()?;
        let operator: WithSpan<BinaryOperator> = WithSpan::new(
            Into::<TokenKind>::into(next_token)
                .try_into()
                .expect("This should only be binary operator, you fucked up"),
            next_token.span.clone(),
        );
        let right = self.parse_expression(precedence)?;
        let span = Span::union(&left, &right);

        let binary = BinaryExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        };

        Some(WithSpan::new(Expression::Binary(binary), span))
    }

    fn parse_logical(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        let precedence: Precedence = Precedence::from(self.peek_kind());
        let next_token = &self.next()?;
        let operator: WithSpan<LogicalOperator> = WithSpan::new(
            Into::<TokenKind>::into(next_token)
                .try_into()
                .expect("This should only be binary operator, you fucked up"),
            next_token.span.clone(),
        );

        let right = self.parse_expression(precedence)?;
        let span = Span::union(&left, &right);
        let logical = LogicalExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        };

        Some(WithSpan::new(Expression::Logical(logical), span))
    }

    fn parse_assign(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        self.next()
            .expect("Should only be called if the next token is assign");
        let right = self.parse_expression(Precedence::None)?;

        let span = Span::union(&left, &right);
        let assign = AssignExpression {
            left: Box::new(left),
            right: Box::new(right),
        };

        Some(WithSpan::new(Expression::Assign(assign), span))
    }

    fn parse_call(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        self.next()
            .expect("Should only be called if next token is left paren");
        let arguments = self.parse_list_items(TokenKind::RParen);
        let right_paren = self.consume_next_if(TokenKind::RParen)?;
        let span = Span::union(&left, &right_paren);
        let call = CallExpression {
            function: Box::new(left),
            arguments,
        };

        Some(WithSpan::new(Expression::Call(call), span))
    }

    fn parse_property(&mut self, left: WithSpan<Expression>) -> Option<WithSpan<Expression>> {
        self.next()
            .expect("Should only be called if next token is dot");
        let expression = self.parse_expression(Precedence::None)?;

        match expression.val {
            Expression::Identifier(_) => {
                let span = Span::union(&left, &expression);

                let property = PropertyExpression {
                    identifier: Box::new(left),
                    property: Box::new(expression),
                };

                Some(WithSpan::new(Expression::Property(property), span))
            }
            _ => {
                self.errors(
                    &format!("Expected identifier, got {:?}", expression.val),
                    expression.span,
                );
                None
            }
        }
    }

    fn consume_next_if(&mut self, kind: TokenKind) -> Option<WithSpan<Token>> {
        if self.check_next_kind(|k| k == kind) {
            Some(self.next().unwrap()) // only consumes the right token
        } else {
            let peek_token = &self.peek_token().unwrap_or(EOF_TOKEN);
            self.errors(
                &format!(
                    "Expected {}, got {}",
                    kind,
                    Into::<TokenKind>::into(peek_token)
                ),
                peek_token.span.clone(),
            );
            None
        }
    }

    fn expect_ident(&mut self) -> Option<WithSpan<Expression>> {
        let next_token = self.next().unwrap_or(EOF_TOKEN);

        match next_token.val {
            Token::Identifier(ident) => Some(WithSpan::new(
                Expression::Identifier(ident),
                next_token.span,
            )),
            a => {
                self.errors(&format!("Expected identifier, got: {}", a), next_token.span);
                None
            }
        }
    }
}
