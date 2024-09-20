use crate::token::TokenKind;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    None,
    Assign,
    Or,
    And,
    Eq,
    Ord,
    Term,
    Factor,
    Unary,
    Call,
    Index,
    Primary,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::NotEqual | TokenKind::Equal => Self::Eq,
            TokenKind::LessEqual
            | TokenKind::Less
            | TokenKind::Greater
            | TokenKind::GreaterEqual => Self::Ord,
            TokenKind::Plus | TokenKind::Minus => Self::Term,
            TokenKind::Bang => Self::Unary,
            TokenKind::Asterisk | TokenKind::Slash => Self::Factor,
            TokenKind::LParen => Self::Call,
            TokenKind::Dot => Self::Call,
            TokenKind::Assign => Self::Assign,
            TokenKind::Or => Self::Or,
            TokenKind::And => Self::And,
            TokenKind::LBracket => Self::Index,
            _ => Self::None,
        }
    }
}
