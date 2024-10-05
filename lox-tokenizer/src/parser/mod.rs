use core::panic;
use std::{iter::Peekable, vec::IntoIter};

pub mod ast;
pub(crate) mod precedence;
use ast::{
    AssignExpression, BinaryExpression, BinaryOperator, BlockStatement, CallExpression,
    ClassStatement, Expression, ExtendsExpression, ForExpression, FunctionStatement,
    GroupingExpression, IfExpression, IndexExpression, LetStatement, ListExpression,
    LogicalExpression, LogicalOperator, PropertyExpression, ReturnStatement, Statement,
    SuperExpression, UnaryExpression, UnaryOperator, WhileExpression,
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
    #[allow(unused)] // we have some optional methods for now...
    pub fn from_code(code: &str) -> Self {
        let errors = Vec::default();
        let tokens_iter = Lexer::new(code).lex_with_context().into_iter().peekable();

        Self {
            tokens_iter,
            errors,
            cursor: 0,
        }
    }

    #[allow(unused)]
    pub fn from_token_span(token_span: Vec<WithSpan<Token>>) -> Self {
        Self {
            tokens_iter: token_span.into_iter().peekable(),
            errors: Vec::default(),
            cursor: 0,
        }
    }

    #[allow(unused)]
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

    #[allow(unused)]
    pub fn get_errors(&self) -> Vec<Diagnostic> {
        self.errors.clone()
    }

    pub fn parse_program(&mut self) -> Vec<WithSpan<Statement>> {
        let mut statements: Vec<WithSpan<Statement>> = Vec::default();
        while let Some(token) = self.peek_token() {
            if token.val == Token::Eof {
                break;
            }
            if let Some(statement) = self.parse_statement(token) {
                statements.push(statement);
                continue;
            }
            self.next();
        }
        statements
    }

    fn parse_statement(&mut self, token: WithSpan<Token>) -> Option<WithSpan<Statement>> {
        match Into::<TokenKind>::into(token) {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Function => self.parse_function_statement(),
            TokenKind::Class => self.parse_class_statement(),
            TokenKind::LBrace => self.parse_block_statement(),
            _ => self.parse_expresion_statement(),
        }
    }

    // statement parser

    fn parse_let_statement(&mut self) -> Option<WithSpan<Statement>> {
        let let_token = self
            .next()
            .expect("Should only be called if the next token is let token");

        let identifier = self.expect_ident()?;

        if self.check_next_kind(|k| k != TokenKind::Assign) {
            let semicolon = self.expect_next(TokenKind::Semicolon)?;
            let let_statement = LetStatement {
                identifier,
                initializer: None,
            };
            return Some(WithSpan::new(
                Statement::Let(let_statement),
                Span::union(&let_token, &semicolon),
            ));
        }

        self.expect_next(TokenKind::Assign)?;

        let initializer = Some(self.parse_expression(Precedence::None)?);
        let end = self.expect_next(TokenKind::Semicolon)?;

        let let_statement = LetStatement {
            identifier,
            initializer,
        };

        Some(WithSpan::new(
            Statement::Let(let_statement),
            Span::union(&let_token, &end),
        ))
    }

    fn parse_expresion_statement(&mut self) -> Option<WithSpan<Statement>> {
        let expression = self.parse_expression(Precedence::None)?;
        let semicolon = self.expect_next(TokenKind::Semicolon)?;

        let span = Span::union(&expression, &semicolon);
        let expr_statement = Statement::Expression(expression);

        Some(WithSpan::new(expr_statement, span))
    }

    fn parse_return_statement(&mut self) -> Option<WithSpan<Statement>> {
        let return_token = self
            .next()
            .expect("Should only be called if the next token is return token");

        let return_value = self.parse_expression(Precedence::None);

        let end = self.expect_next(TokenKind::Semicolon)?;
        let return_statement = ReturnStatement { return_value };

        Some(WithSpan::new(
            Statement::Return(return_statement),
            Span::union(&return_token, &end),
        ))
    }

    fn parse_class_statement(&mut self) -> Option<WithSpan<Statement>> {
        let class_token = self
            .next()
            .expect("Should only be called if the next token is class token");
        let identifier = self.expect_ident()?;
        let mut extends: Option<WithSpan<Expression>> = None;

        if self.check_next_kind(|k| k == TokenKind::Extends) {
            extends = self.parse_extends();
        }

        let body = Box::new(self.parse_block_statement()?);

        let end = self.expect_next(TokenKind::Semicolon)?;

        let span = Span::union(&class_token, &end);

        let class_statement = ClassStatement {
            identifier,
            extends,
            body,
        };

        Some(WithSpan::new(Statement::Class(class_statement), span))
    }

    fn parse_block_statement(&mut self) -> Option<WithSpan<Statement>> {
        let open_brace = self.expect_next(TokenKind::LBrace)?;
        let mut statements: Vec<WithSpan<Statement>> = Vec::default();
        let mut end: WithSpan<Token> = EOF_TOKEN;

        while let Some(token) = self.peek_token() {
            if Into::<TokenKind>::into(&token.val) == TokenKind::RBrace
                || Into::<TokenKind>::into(&token.val) == TokenKind::Eof
            {
                end = self.next().unwrap();
                break;
            }

            if let Some(statement) = self.parse_statement(token) {
                statements.push(statement);
            };
        }

        let span = Span::union(&open_brace, &end);
        let block_statement = BlockStatement { statements };

        Some(WithSpan::new(Statement::Block(block_statement), span))
    }

    fn parse_function_statement(&mut self) -> Option<WithSpan<Statement>> {
        let function_token = self
            .next()
            .expect("Should only be called if the next token is  function token");
        let identifier = self.expect_ident()?;
        self.expect_next(TokenKind::LParen)?;
        let parameters = self.parse_function_parameters();
        self.expect_next(TokenKind::RParen)?;
        let statement = Box::new(self.parse_block_statement()?);

        let end = self.expect_next(TokenKind::Semicolon)?;

        let span = Span::union(&function_token, &end);

        let function_statement = FunctionStatement {
            identifier,
            parameters,
            statement,
        };

        Some(WithSpan::new(Statement::Function(function_statement), span))
    }

    fn parse_function_parameters(&mut self) -> Vec<WithSpan<Expression>> {
        let mut parameters: Vec<WithSpan<Expression>> = Vec::default();

        if self.check_next_kind(|k| k != TokenKind::RParen) {
            if let Some(param) = self.expect_ident() {
                parameters.push(param);
            };

            while self.check_next_kind(|k| k == TokenKind::Comma) {
                self.expect_next(TokenKind::Comma);
                if let Some(param) = self.expect_ident() {
                    parameters.push(param);
                }
            }
        }

        parameters
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
            TokenKind::If => self.parse_if_expresion(),
            TokenKind::For => self.parse_for_expression(),
            TokenKind::While => self.parse_while_expression(),
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
            _ => {
                let peek = self.peek_token().unwrap_or(EOF_TOKEN);
                self.errors(&format!("No infix found for: {}", peek.val), peek.span);
                None
            }
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
    //
    fn parse_if_expresion(&mut self) -> Option<WithSpan<Expression>> {
        let if_token = &self
            .next()
            .expect("Should only be called if the next token is if token");
        self.expect_next(TokenKind::LParen)?; // consumes the next parentheses

        let condition = Box::new(self.parse_expression(Precedence::None)?);

        self.expect_next(TokenKind::RParen)?; // consumes the closing parentheses

        let consequence = Box::new(self.parse_block_statement()?);
        let mut span = Span::union(if_token, &consequence);

        let mut alternative: Option<Box<WithSpan<Statement>>> = None;

        if self.check_next_kind(|f| f == TokenKind::Else) {
            self.next().unwrap();
            let alter = self.parse_block_statement()?;
            span = Span::union(if_token, &alter);
            alternative = Some(Box::new(alter));
        }

        let if_exrp = IfExpression {
            condition,
            consequence,
            alternative,
        };

        Some(WithSpan::new(Expression::If(if_exrp), span))
    }

    fn parse_while_expression(&mut self) -> Option<WithSpan<Expression>> {
        let while_token = &self
            .next()
            .expect("Should only be called if next token if else token");
        self.expect_next(TokenKind::LParen)?;

        let condition = Box::new(self.parse_expression(Precedence::None)?);

        self.expect_next(TokenKind::RParen)?;

        let consequence = Box::new(self.parse_block_statement()?);
        let span = Span::union(while_token, &consequence);

        let while_expr = WhileExpression {
            condition,
            consequence,
        };

        Some(WithSpan::new(Expression::While(while_expr), span))
    }

    fn parse_for_expression(&mut self) -> Option<WithSpan<Expression>> {
        let for_token = self
            .next()
            .expect("Should only be called if next token is for token");

        self.expect_next(TokenKind::LParen)?;
        let initializer = match self.parse_let_statement() {
            Some(let_statement) => Box::new(let_statement),
            None => {
                self.errors("Expected let statement, got <none>", for_token.span);
                return None;
            }
        };
        let condition = Box::new(self.parse_expression(Precedence::None)?);
        self.expect_next(TokenKind::Semicolon)?;

        let assignment = Box::new(self.parse_expression(Precedence::None)?);
        self.expect_next(TokenKind::RParen)?;

        let consequence = Box::new(self.parse_block_statement()?);

        let span = Span::union(&for_token, &consequence);

        let for_expression = ForExpression {
            initializer,
            condition,
            assignment,
            consequence,
        };

        Some(WithSpan::new(Expression::For(for_expression), span))
    }

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
        let right_paren = self.expect_next(TokenKind::RParen)?;
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
        let right_bracket = self.expect_next(TokenKind::RBracket)?;

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
                self.expect_next(TokenKind::Comma).unwrap();
                if let Some(expr) = self.parse_expression(Precedence::None) {
                    elements.push(expr)
                }
            }
        }

        elements
    }

    fn parse_super(&mut self, keyword: &WithSpan<Token>) -> Option<WithSpan<Expression>> {
        self.expect_next(TokenKind::Dot)?;
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

        let right_bracket = self.expect_next(TokenKind::RBracket)?;
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
        let right_paren = self.expect_next(TokenKind::RParen)?;
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

    fn expect_next(&mut self, kind: TokenKind) -> Option<WithSpan<Token>> {
        let next_token = self.next().unwrap_or(EOF_TOKEN);
        let next_kind: TokenKind = next_token.clone().into();
        if next_kind == kind {
            Some(next_token)
        } else {
            self.errors(
                &format!("Expected {}, got {}", kind, next_kind),
                next_token.span,
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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_let_statement_with_initializer() {
        let code = r#"
            let iden = 10;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Let(LetStatement {
            identifier,
            initializer,
        }) = &program[0].val
        {
            assert!(
                matches!(&identifier.val, Expression::Identifier(s) if s == "iden" ),
                "Expected identifier to be identifier expression with value `iden`, got {:?}",
                identifier.val
            );

            assert!(
                initializer.is_some(),
                "Expected let statement to have initialzer, but got none instead"
            );

            assert!(
                matches!(initializer.clone().unwrap().val, Expression::Number(num) if num == 10.0),
                "Expected initializer to be a number expression with value 10.0, but got {:?}",
                initializer.clone().unwrap()
            );
        } else {
            self::panic!("Expected let statement, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_parse_let_statement_without_initializer() {
        let code = r#"
            let a;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Let(LetStatement {
            identifier,
            initializer,
        }) = &program[0].val
        {
            assert!(
                matches!(&identifier.val, Expression::Identifier(s) if s == "a" ),
                "Expected identifier to be identifier expression with value `a`, got {:?}",
                identifier.val
            );

            assert!(
                initializer.is_none(),
                "Expected let statement to have no initialzer, but got some instead"
            );
        } else {
            self::panic!("Expected let statement, got: {:?}", program[0].val)
        }
    }

    // #[test]
    fn test_return_statement_value() {
        let code = r#"
            return b;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );
        if let Statement::Return(ReturnStatement { return_value }) = &program[0].val {
            assert!(
                return_value.is_some(),
                "Epxtected to have some return value, but got none"
            );

            assert!(
                matches!(return_value.clone().unwrap().val, Expression::Identifier(i) if i == "b"),
                "Exptected return value to be an identifier with value `b`, but got {:?}",
                return_value.clone().unwrap()
            );
        } else {
            self::panic!("Expected to have a return statement, but got none")
        }
    }

    #[test]
    fn test_return_statement_expression() {
        let code = r#"
            return a + b;
        "#;
        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Return(ReturnStatement { return_value }) = &program[0].val {
            assert!(
                return_value.is_some(),
                "Epxtected to have some return value, but got none"
            );

            let (left, operator, right) = match return_value.clone().unwrap().val {
                Expression::Binary(BinaryExpression {
                    left,
                    operator,
                    right,
                }) => (left, operator, right),
                _ => self::panic!(
                    "Expected to have a logical expression as return value, but got: {:?}",
                    return_value.clone().unwrap().val
                ),
            };

            assert!(
                matches!(left.val, Expression::Identifier(ref a) if a == "a"),
                "Exptected return value left of operation to be identifier `a`, but got: {:?}",
                left.val
            );

            assert!(
                matches!(operator.val, BinaryOperator::Plus),
                "Expected operator to be {:?}, but got: {:?}",
                BinaryOperator::Plus,
                operator.val
            );

            assert!(
                matches!(right.val, Expression::Identifier(ref b) if b == "b"),
                "Exptected return value right of operation to be identifier `b`, but got: {:?}",
                left.val
            )
        } else {
            self::panic!("Expected to have a return statement, but got none")
        }
    }

    #[test]
    fn test_parse_function_statement_with_return() {
        let code = r#"
            fun add(a, b) {
                let a = 1;
                let b = 2;

                return a + b;
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Function(FunctionStatement {
            identifier,
            parameters,
            statement,
        }) = &program[0].val
        {
            assert!(
                matches!(identifier.val, Expression::Identifier(ref i) if i == "add"),
                "Expected function identifier to be `i`, but got: {:?}",
                identifier.val
            );
            assert!(
                parameters.len() == 2,
                "Expected function to have 2 parameteres, but got: {}",
                parameters.len()
            );
            if let Statement::Block(BlockStatement { statements }) = &statement.val {
                assert!(
                    statements.len() == 3,
                    "Expected function body to have 3 statements, but got: {}",
                    statements.len()
                );

                assert!(
                    matches!(statements[2].val, Statement::Return(_)),
                    "Expected return statement, but got: {:?}",
                    statements[2].val
                )
            } else {
                self::panic!(
                    "Expected function body to be block statement, but got: {:?}",
                    statement.val
                );
            }
        } else {
            self::panic!(
                "Expected to be function statement, but got: {:?}",
                program[0]
            )
        }
    }

    #[test]
    fn test_parse_function_statement_without_return() {
        let code = r#"
            fun add(a, b) {
                let a = 1;
                let b = 2;
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Function(FunctionStatement {
            identifier,
            parameters,
            statement,
        }) = &program[0].val
        {
            assert!(
                matches!(identifier.val, Expression::Identifier(ref i) if i == "add"),
                "Expected function identifier to be `i`, but got: {:?}",
                identifier.val
            );
            assert!(
                parameters.len() == 2,
                "Expected function to have 2 parameteres, but got: {}",
                parameters.len()
            );
            if let Statement::Block(BlockStatement { statements }) = &statement.val {
                assert!(
                    statements.len() == 2,
                    "Expected function body to have 3 statements, but got: {}",
                    statements.len()
                );

                assert!(
                    matches!(statements[1].val, Statement::Let(_)),
                    "Expected let statement, but got: {:?}",
                    statements[1].val
                )
            } else {
                self::panic!(
                    "Expected function body to be block statement, but got: {:?}",
                    statement.val
                );
            }
        } else {
            self::panic!(
                "Expected to be function statement, but got: {:?}",
                program[0]
            )
        }
    }

    #[test]
    fn test_class_statement_without_extends() {
        let code = r#"
            class Thomas {
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Class(ClassStatement {
            identifier,
            extends,
            body,
        }) = &program[0].val
        {
            assert!(
                matches!(identifier.val, Expression::Identifier(ref i) if i == "Thomas"),
                "Expected class name to be `Thomas`, got: {:?}",
                identifier.val
            );
            assert!(
                extends.is_none(),
                "Expected class to not extends other class, but got: {:?}",
                extends.clone().unwrap()
            );
            assert!(
                matches!(body.val, Statement::Block(_)),
                "Expected class body to be block statement, but got: {:?}",
                body.val
            )
        } else {
            self::panic!("Expected class statement, got: {:?}", program[0])
        }
    }

    #[test]
    fn test_function_with_extends() {
        let code = r#"
            class Thomas extends Jerry {
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Class(ClassStatement {
            identifier,
            extends,
            body,
        }) = &program[0].val
        {
            assert!(
                matches!(identifier.val, Expression::Identifier(ref i) if i == "Thomas"),
                "Expected class name to be `Thomas`, got: {:?}",
                identifier.val
            );
            assert!(
                extends.is_some(),
                "Expected class to  extends other class, but got none",
            );
            assert!(
                matches!(body.val, Statement::Block(_)),
                "Expected class body to be block statement, but got: {:?}",
                body.val
            )
        } else {
            self::panic!("Expected class statement, got: {:?}", program[0])
        }
    }

    #[test]
    fn test_if_only() {
        let code = r#"
            if (a < b) {
                return a;
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::If(IfExpression {
                    condition,
                    consequence,
                    alternative,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                alternative.is_none(),
                "Expected if expression to have no alternative flow, but got some"
            );

            assert!(
                matches!(condition.val, Expression::Binary(_)),
                "Expected condition to be binary expression, but got: {:?}",
                consequence.val
            );

            assert!(
                matches!(consequence.val, Statement::Block(_)),
                "Epxected consequence to be a block statement, but got: {:?}",
                consequence.val
            )
        } else {
            self::panic!("Expected to be if expression, got: {:?}", program[0].val);
        }
    }

    #[test]
    fn test_if_else() {
        let code = r#"
            if (a < b) {
                b - a;
            } else {
                a - b;
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::If(IfExpression {
                    condition,
                    consequence,
                    alternative,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                alternative.is_some(),
                "Expected if expression to have some alternative flow, but got none"
            );

            assert!(
                matches!(alternative.clone().unwrap().val, Statement::Block(_)),
                "Expected alternative flow to be block statement, but got: {:?}",
                alternative.clone().unwrap()
            );

            assert!(
                matches!(condition.val, Expression::Binary(_)),
                "Expected condition to be binary expression, but got: {:?}",
                consequence.val
            );

            assert!(
                matches!(consequence.val, Statement::Block(_)),
                "Epxected consequence to be a block statement, but got: {:?}",
                consequence.val
            )
        } else {
            self::panic!("Expected to be if expression, got: {:?}", program[0].val);
        }
    }
    #[test]
    fn test_while() {
        let code = r#"
            while (x < 10) {
                x = x + 1;
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::While(WhileExpression {
                    condition,
                    consequence,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                matches!(condition.val, Expression::Binary(_)),
                "Expected condition to be binary expression, but got: {:?}",
                consequence.val
            );

            assert!(
                matches!(consequence.val, Statement::Block(_)),
                "Epxected consequence to be a block statement, but got: {:?}",
                consequence.val
            )
        } else {
            self::panic!("Expected while expression, but got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_identifier_expression() {
        let code = r#"
            thomas;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::Identifier(s),
                    ..
                }) if s == "thomas"
            ),
            "Expected identifier expression statement with value `thomas`, got: {:?}",
            program[0].val
        )
    }

    #[test]
    fn test_number_expresion() {
        let code = r#"
            100.055;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::Number(n),
                    ..
                }) if *n == 100.055
            ),
            "Expected to have a number expression statement, but got: {:?}",
            program[0].val
        )
    }

    #[test]
    fn test_string_expression() {
        let code = r#"
            "thomas";
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::String(s),
                    ..
                }) if s == "thomas"
            ),
            "Expected to have a string expression statement, but got: {:?}",
            program[0].val
        )
    }

    #[test]
    fn test_boolean_expresion() {
        let code = r#"
            true;
            false;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();

        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::Boolean(true),
                    ..
                })
            ),
            "Expected to have a true boolean expression statement, but got: {:?}",
            program[0].val
        );

        assert!(
            matches!(
                &program[1].val,
                Statement::Expression(WithSpan {
                    val: Expression::Boolean(false),
                    ..
                })
            ),
            "Expected to have a false boolean expression statement, but got: {:?}",
            program[1].val
        )
    }

    #[test]
    fn test_nil_expression() {
        let code = r#"
            nil;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::Nil,
                    ..
                })
            ),
            "Expected to have a nil expression statement, but got: {:?}",
            program[0].val
        )
    }

    #[test]
    fn test_logical_expression() {
        let code = r#"
            a && b;
            b || c;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        let first = match program[0].val {
            Statement::Expression(ref e) => match e.val {
                Expression::Logical(ref logical) => logical,
                _ => self::panic!("Expected first to be logical expression"),
            },
            _ => self::panic!("Expected first statement to be an expression statement"),
        };

        assert!(
            matches!(&first.left.val, Expression::Identifier(a) if a == "a"),
            "Expected to be identifier expression, got {:?}",
            first.left.val
        );

        assert!(
            matches!(&first.operator.val, LogicalOperator::And),
            "Expected to be logical operator `and`, got {:?}",
            first.operator.val
        );

        assert!(
            matches!(&first.right.val, Expression::Identifier(b) if b == "b"),
            "Expected to be identifier expression, got {:?}",
            first.right.val
        );

        let second = match program[1].val {
            Statement::Expression(ref e) => match e.val {
                Expression::Logical(ref logical) => logical,
                _ => self::panic!("Expected first to be logical expression"),
            },
            _ => self::panic!("Expected first statement to be an expression statement"),
        };

        assert!(
            matches!(&second.left.val, Expression::Identifier(b) if b == "b"),
            "Expected to be identifier expression, got {:?}",
            second.left.val
        );

        assert!(
            matches!(&second.operator.val, LogicalOperator::Or),
            "Expected to be logical operator `and`, got {:?}",
            second.operator.val
        );

        assert!(
            matches!(&second.right.val, Expression::Identifier(c) if c == "c"),
            "Expected to be identifier expression, got {:?}",
            second.right.val
        );
    }

    #[test]
    fn test_binary_expression() {
        let code = r#"
            a + b;
            a - b;
            b * c;
            c < b;
            a > b;
            10 >= 5;
            5 <= 5;
            10 != 5;
            10 == 10;
            a / b;
        "#;
        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        type Expected = (Expression, BinaryOperator, Expression);

        let expected: Vec<Expected> = vec![
            (
                Expression::Identifier("a".to_owned()),
                BinaryOperator::Plus,
                Expression::Identifier("b".to_string()),
            ),
            (
                Expression::Identifier("a".to_string()),
                BinaryOperator::Minus,
                Expression::Identifier("b".to_string()),
            ),
            (
                Expression::Identifier("b".to_string()),
                BinaryOperator::Product,
                Expression::Identifier("c".to_string()),
            ),
            (
                Expression::Identifier("c".to_string()),
                BinaryOperator::Less,
                Expression::Identifier("b".to_string()),
            ),
            (
                Expression::Identifier("a".to_string()),
                BinaryOperator::Greater,
                Expression::Identifier("b".to_string()),
            ),
            (
                Expression::Number(10.0),
                BinaryOperator::GreaterEqual,
                Expression::Number(5.0),
            ),
            (
                Expression::Number(5.0),
                BinaryOperator::LessEqual,
                Expression::Number(5.0),
            ),
            (
                Expression::Number(10.0),
                BinaryOperator::NotEqual,
                Expression::Number(5.0),
            ),
            (
                Expression::Number(10.0),
                BinaryOperator::Equal,
                Expression::Number(10.0),
            ),
            (
                Expression::Identifier("a".to_string()),
                BinaryOperator::Divide,
                Expression::Identifier("b".to_string()),
            ),
        ];

        for (i, v) in program.iter().enumerate() {
            if let Statement::Expression(WithSpan {
                val:
                    Expression::Binary(BinaryExpression {
                        left,
                        operator,
                        right,
                    }),
                ..
            }) = &v.val
            {
                assert_eq!(
                    left.val, expected[i].0,
                    "Expected: {:?}, got: {:?}",
                    expected[i].0, left.val
                );
                assert_eq!(
                    operator.val, expected[i].1,
                    "Expected: {:?}, got: {:?}",
                    expected[i].1, operator.val
                );
                assert_eq!(
                    right.val, expected[i].2,
                    "Expected: {:?}, got: {:?}",
                    expected[i].2, right.val
                );
            } else {
                self::panic!("Expected binary expression, got: {:?}", v.val)
            }
        }
    }

    #[test]
    fn test_call_expresion_without_arguments() {
        let code = r#"
            add();
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::Call(CallExpression {
                    function,
                    arguments,
                }),
            ..
        }) = &program[0].val
        {
            assert_eq!(
                function.val,
                Expression::Identifier("add".to_string()),
                "Expected function identifier to be `add`, got: {:?}",
                function.val
            );

            assert_eq!(
                arguments.len(),
                0,
                "Expected function to have no arguments, but got: {}",
                arguments.len()
            )
        } else {
            self::panic!("Expected call expresion, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_call_expression_with_args() {
        let code = r#"
            add((1 + 2), (2 + 3), 3);
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::Call(CallExpression {
                    function,
                    arguments,
                }),
            ..
        }) = &program[0].val
        {
            assert_eq!(
                function.val,
                Expression::Identifier("add".to_string()),
                "Expected function identifier to be `add`, got: {:?}",
                function.val
            );

            assert_eq!(
                arguments.len(),
                3,
                "Expected function to have 3 arguments, but got: {}",
                arguments.len()
            )
        } else {
            self::panic!("Expected call expresion, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_assign_expresion() {
        let code = r#"
            a = (10 + 5);
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val: Expression::Assign(AssignExpression { left, right }),
            ..
        }) = &program[0].val
        {
            assert_eq!(
                left.val,
                Expression::Identifier("a".to_string()),
                "Expected identifier to be assigned to be `a`. Got: {:?}",
                left.val
            );
            assert!(
                matches!(&right.val, Expression::Grouping(_)),
                "Expected grouping, but found: {:?}",
                right.val
            )
        } else {
            self::panic!("Expected assign expression, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_index_expresion() {
        let code = r#"
            array[(10 + 2)];
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val: Expression::Index(IndexExpression { left, index }),
            ..
        }) = &program[0].val
        {
            assert_eq!(
                left.val,
                Expression::Identifier("array".to_string()),
                "Epxected identifier with value `array, got: {:?}`",
                left.val
            );
            assert!(
                matches!(index.val, Expression::Grouping(_)),
                "Expected index value to be grouping expression, got: {:?}",
                index.val
            )
        } else {
            self::panic!("Expected index expression, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_grouping_expresion() {
        let code = r#"
            ((a * b) + c);
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val: Expression::Grouping(GroupingExpression { expression }),
            ..
        }) = &program[0].val
        {
            assert!(
                matches!(expression.val, Expression::Binary(_)),
                "Expected to be binary, got: {:?}",
                expression.val
            )
        } else {
            self::panic!("Epxected grouping expression, got: {:?}", program[0].val)
        };
    }

    #[test]
    fn test_list_expresion() {
        let code = r#"[1, 2, 3, 4];"#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val: Expression::List(ListExpression { elements }),
            ..
        }) = &program[0].val
        {
            assert_eq!(
                elements.len(),
                4,
                "Expected the array to have 4 elements, got: {}",
                elements.len()
            );

            for (i, val) in elements.iter().enumerate() {
                assert_eq!(
                    val.val,
                    Expression::Number((i + 1) as f64),
                    "Expected to be number: {}, got: {:?}",
                    (i + 1),
                    val.val
                )
            }
        } else {
            self::panic!("Expected list expression, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_super_expresion() {
        let code = r#"super.init(a, b);"#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::Call(CallExpression {
                    function,
                    arguments,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                matches!(
                    &function.val,
                    Expression::Super(SuperExpression { identifier })
                    if identifier.val == Expression::Identifier("init".to_string())
                ),
                "Expected method to be `init`, got: {:?}",
                function.val
            );
            assert_eq!(
                arguments.len(),
                2,
                "Expected program to take 2 arguments, got: {}",
                arguments.len()
            )
        } else {
            self::panic!("Expected call expression, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_unary_expresion() {
        let code = r#"
            -5;
            !(10 <= 5);
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        for i in &program {
            assert!(
                matches!(
                    i.val,
                    Statement::Expression(WithSpan {
                        val: Expression::Unary(_),
                        ..
                    })
                ),
                "Expected unary expression, got: {:?}",
                i.val
            )
        }
        assert!(
            matches!(
                &program[0].val,
                Statement::Expression(WithSpan {
                    val: Expression::Unary(UnaryExpression {
                        operator,
                        expression
                    }),
                    ..
                }) if operator.val == UnaryOperator::Minus && matches!(expression.val, Expression::Number(5.0))
            ),
            "Expected unary expression: -5, got: {:?}",
            program[0].val
        );
        assert!(
            matches!(
                &program[1].val,
                Statement::Expression(WithSpan {
                    val: Expression::Unary(UnaryExpression {
                        operator,
                        expression
                    }),
                    ..
                }) if operator.val == UnaryOperator::Bang && matches!(&expression.val, Expression::Grouping(_))
            ),
            "Expected unary expression: !(10 <= 5), got: {:?}",
            program[1].val
        )
    }

    #[test]
    fn test_this_expresion() {
        let code = r#"
            this.name;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::Property(PropertyExpression {
                    identifier,
                    property,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                matches!(identifier.val, Expression::This),
                "Expected This, got: {:?}",
                identifier.val
            );
            assert_eq!(
                property.val,
                Expression::Identifier("name".to_string()),
                "Expected identifier `name`, got: {:?}",
                property.val
            );
        } else {
            self::panic!("Expected property expression, got: {:?}", program[0].val)
        }
    }

    #[test]
    fn test_for_expression() {
        let code = r#"
            for (let x = 0; x < 10; x = x + 1) {
                add(x);
            };
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Expression(WithSpan {
            val:
                Expression::For(ForExpression {
                    initializer,
                    condition,
                    assignment,
                    consequence,
                }),
            ..
        }) = &program[0].val
        {
            assert!(
                matches!(initializer.val, Statement::Let(_)),
                "Expected initializer to be let statement, got: {:?}",
                initializer.val
            );

            assert!(
                matches!(condition.val, Expression::Binary(_)),
                "Expected condition to be binary expression, got: {:?}",
                condition.val
            );

            assert!(
                matches!(assignment.val, Expression::Assign(_)),
                "Expected assignment to be assign expression, got: {:?}",
                assignment.val
            );
            assert!(matches!(consequence.val, Statement::Block(_)))
        } else {
            self::panic!("Expected for expression, got: {:?}", program[0])
        }
    }

    #[test]
    fn test_block_statement() {
        let code = r#"
            {
                let add = 10;
                let b = 20;
            }
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser.parse_program();
        assert!(
            parser.get_errors().is_empty(),
            "Expected progarm to have no errors, but got: {} instead",
            parser.get_errors().len()
        );

        if let Statement::Block(BlockStatement { statements }) = &program[0].val {
            assert!(
                statements.len() == 2,
                "Expected block to hava 2 statements, got: {}",
                statements.len()
            );
        } else {
            self::panic!("Expected block statement, got: {:?}", program[0].val)
        }
    }
}
