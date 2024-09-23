use std::fmt::{write, Display};

use crate::token::{positions::WithSpan, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(WithSpan<Expression>),
    Block(BlockStatement),
    Function(FunctionStatement),
    Class(ClassStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // Literal Expression
    Identifier(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    This,
    If(IfExpression),
    For(ForExpression),
    While(WhileExpression),
    Super(SuperExpression),
    // Unary
    Unary(UnaryExpression),
    // Logical
    Logical(LogicalExpression),
    // Binary
    Binary(BinaryExpression),
    Assign(AssignExpression),
    // Call and indexing
    Call(CallExpression),
    Index(IndexExpression),
    Extends(ExtendsExpression),
    Property(PropertyExpression),
    Grouping(GroupingExpression),
    List(ListExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassStatement {
    pub identifier: WithSpan<Expression>,
    pub extends: Option<WithSpan<Expression>>,
    pub body: Box<WithSpan<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement {
    pub identifier: WithSpan<Expression>,
    pub parameters: Vec<WithSpan<Expression>>,
    pub statement: Box<WithSpan<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<WithSpan<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub return_value: Option<WithSpan<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub identifier: WithSpan<Expression>,
    pub initializer: Option<WithSpan<Expression>>, // This should only be none when we doesnt have
                                                   // assign after identifier
}

/// A List of expression in []
///
/// * `elements`: expressions
#[derive(Debug, Clone, PartialEq)]
pub struct ListExpression {
    pub elements: Vec<WithSpan<Expression>>,
}
/// Grouping expression is a statement grouped together in ()
///
/// * `expression`: Expression inside ()
#[derive(Debug, Clone, PartialEq)]
pub struct GroupingExpression {
    pub expression: Box<WithSpan<Expression>>,
}

/// Property expressions take this representation
/// ```text
///     iden.val;
///      ^    ^_ _ _
///  identifier     |
///              property
/// ```             
///
/// * `identifier`: Identifier
/// * `property`: Identifier of property
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyExpression {
    pub identifier: Box<WithSpan<Expression>>,
    pub property: Box<WithSpan<Expression>>,
}

/// Extends expressions take this representation
/// ```text
///     class Cat extends Animals {
///         ...     ^_ _ _ _ ^
///     }           |        |
///                 |    Identifier
///          extends keyword
/// ```
///
/// * `identifier`: Inherited class Identifier
#[derive(Debug, Clone, PartialEq)]
pub struct ExtendsExpression {
    pub child: Box<WithSpan<Expression>>,
}

/// For expression take this representation
/// ```text
///     for (let x = 0; x < 10; x = x + 1) {
///             ^_ _ _ _ _
///         initializer   ^_ _ _ _ _^
///                       |         |
///                   condition  assignment
///         ... -> consequence block statement
///     };
/// ```
///
/// * `initializer`: Intializer variable used by the for loop
/// * `condition`: Condition for the initialized variable
/// * `asignment`: Expression performed to the variable
/// * `consequence`: Block statement
#[derive(Debug, Clone, PartialEq)]
pub struct ForExpression {
    pub initializer: Box<WithSpan<Statement>>,
    pub condition: Box<WithSpan<Expression>>,
    pub assignment: Box<WithSpan<Expression>>,
    pub consequence: Box<WithSpan<Statement>>,
}

/// While expressions take this representation
/// ```text
///     while (n < 0) {
///             ^
///         condition
///         ... -> consequence
///     }
/// ```
///
/// * `condition`: Condition for the while loop
/// * `consequence`: Block statement
#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpression {
    pub condition: Box<WithSpan<Expression>>,
    pub consequence: Box<WithSpan<Statement>>,
}

/// If expression take this representation
/// ```text
///         if (1 < 0) { _ _ _ _ _ _ Statement
///             return true;
///         } else { _ _ _ _ _ _ _ _ Statement
///             return false;
///         }
/// ```
///
/// * `condition`: Condition after if keyword
/// * `consequence`: Block of code execute if true
/// * `alternative`: Alternate if false
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<WithSpan<Expression>>,
    pub consequence: Box<WithSpan<Statement>>,
    pub alternative: Option<Box<WithSpan<Statement>>>,
}

/// Super expression take this representation
/// ```text
///     super(age, name);
///
///     ^-----^-----^
///     |     |     |
/// identifier|     |-----
///       expression     |
///                 expression
/// ```
///
/// * `arguments`: A list of expression
#[derive(Debug, Clone, PartialEq)]
pub struct SuperExpression {
    pub identifier: Box<WithSpan<Expression>>,
}

/// An index expression takes this represtation
///```text
///         a[(1 + 2)]
///         ^- - -^- - - - -
///         |              |
/// identifier expression  |
///                  Index expression
///```
///Or it could takes more complex format like this
///```text
///         fun add(a,b) {return [a,b];};
///         fun minus(a,b) {return a - b;};
///
///         add(3,4)[minus(4,3)];
///            ^- - - - ^ - - - - -
///            |                  |
/// function call as expression   |
///                 function call as index expression
///
///```      
///
/// * `left`: Identifer or expression identifier
/// * `index`: Index expression
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub left: Box<WithSpan<Expression>>,
    pub index: Box<WithSpan<Expression>>,
}

/// Call Expressions take this represetation
/// ```text
///         add((1 + 2 + 3), (3 -4));
///          ^_ _ _ _ _ _^
///          |           |
/// Function expression  |
///             arguments expression
/// ```
///
/// * `function`: Function expression
/// * `arguments`: List of expression as arguments
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub function: Box<WithSpan<Expression>>,
    pub arguments: Vec<WithSpan<Expression>>,
}

/// Assign expressions take this representation
/// ```markdown
///     let x = add(a,b);
///         ^_ _ _ _^
///         |       |
///     identifier  |
///             assigned value
/// ```
/// * `identifier`: Identifier expession
/// * `expression`: Expression as value assigned
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpression {
    pub left: Box<WithSpan<Expression>>,
    pub right: Box<WithSpan<Expression>>,
}

/// Unary expressions take this representation
/// ```text
///         !true
///         ^_^_ _ _
///         |        |
///   Unary operator
///              Expression
/// ```
///
/// * `operator`: Unary operator, we currently have `-`, `!`
/// * `expression`: Expression
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: Box<WithSpan<UnaryOperator>>,
    pub expression: Box<WithSpan<Expression>>,
}

/// Logical expressions take this representation
/// ```text
///     (1 + 2 + 3) < (3 + 4)
///         ^_ _ _ _^_ _ ^
///         |       |_ _ |_ _
///  Logical expression |    |
///                     |    |_  
///                 Operator   |
///                     Logical expression       
/// ```
/// * `left`: Left expression
/// * `operator`: Logical operator
/// * `right`: Right epxression
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpression {
    pub left: Box<WithSpan<Expression>>,
    pub operator: WithSpan<LogicalOperator>,
    pub right: Box<WithSpan<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
/// Binary Expressions take this representation
/// ```text
///     (1 + 2 + 3) && (3 + 4)
///         ^_ _ _ _^_ _ ^
///         |       |_ _ |_ _
///  Logical expression |    |
///                     |    |_  
///                 Operator   |
///                     Logical expression       
/// ```
///
/// * `left`: Left expression
/// * `operator`: Binary operator
/// * `right`: Right epxression
pub struct BinaryExpression {
    pub left: Box<WithSpan<Expression>>,
    pub operator: WithSpan<BinaryOperator>,
    pub right: Box<WithSpan<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum LogicalOperator {
    And, // &&
    Or,  // ||
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Product,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl TryFrom<TokenKind> for UnaryOperator {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::Minus => Ok(Self::Minus),
            _ => Err(format!("Could not convert {} to unary operator", value)),
        }
    }
}

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Asterisk => Ok(Self::Product),
            TokenKind::Slash => Ok(Self::Divide),
            TokenKind::Equal => Ok(Self::Equal),
            TokenKind::NotEqual => Ok(Self::NotEqual),
            TokenKind::LessEqual => Ok(Self::LessEqual),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEqual => Ok(Self::GreaterEqual),
            _ => Err(format!("Could not convert {} to binary operator", {
                value
            })),
        }
    }
}

impl TryFrom<TokenKind> for LogicalOperator {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::And => Ok(Self::And),
            TokenKind::Or => Ok(Self::Or),
            _ => Err(format!("Could not convert {} to logical operator", value)),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "<ident:{i}>"),
            Expression::Number(n) => write!(f, "<num:{n}>"),
            Expression::String(s) => write!(f, "<str:{s}>"),
            Expression::Boolean(b) => write!(f, "<bool:{b}>"),
            Expression::Nil => write!(f, "<nil>"),
            Expression::This => write!(f, "<this>"),
            Expression::If(if_expr) => write!(f, "<if:{if_expr}>"),
            Expression::For(for_expr) => write!(f, "<for:{for_expr}>"),
            Expression::While(while_expr) => write!(f, "<while:{while_expr}>"),
            Expression::Super(super_expr) => write!(f, "<super:{super_expr}>"),
            Expression::Unary(unary_expr) => write!(f, "<unary:{unary_expr}>"),
            Expression::Logical(logical_expr) => write!(f, "<logical:{logical_expr}>"),
            Expression::Binary(binary_expr) => write!(f, "<binary:{binary_expr}>"),
            Expression::Assign(assign_expr) => write!(f, "<assign:{assign_expr}>"),
            Expression::Call(call_expr) => write!(f, "<call:{call_expr}>"),
            Expression::Index(index_expr) => write!(f, "<index:{index_expr}>"),
            Expression::Extends(extends_expr) => write!(f, "<extends:{extends_expr}>"),
            Expression::Property(property_expr) => write!(f, "<property:{property_expr}>"),
            Expression::Grouping(grouping_expr) => write!(f, "<grouping:{grouping_expr}>"),
            Expression::List(list_expr) => write!(f, "<list:{list_expr}>"),
        }
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(let_stm) => write!(f, "<let:{let_stm}>"),
            Statement::Return(return_stm) => write!(f, "<return:{return_stm}>"),
            Statement::Expression(expression_stm) => write!(f, "<expression:{expression_stm}>"),
            Statement::Block(block_stm) => write!(f, "<block:{block_stm}>"),
            Statement::Function(function_stm) => write!(f, "<function:{function_stm}>"),
            Statement::Class(class_stm) => write!(f, "<class:{class_stm}>"),
        }
    }
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Product => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
        }
    }
}

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOperator::And => write!(f, "&&"),
            LogicalOperator::Or => write!(f, "||"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Bang => write!(f, "!"),
            UnaryOperator::Minus => write!(f, "-"),
        }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {} = {};",
            self.identifier.val,
            self.initializer
                .clone()
                .unwrap_or(WithSpan::empty(Expression::Nil))
                .val
        )
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        output.push_str("{\n");
        for i in &self.statements {
            output.push_str(&format!("{}", i.val))
        }
        output.push_str("};");
        write!(f, "{}", output)
    }
}
impl Display for FunctionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut param = String::new();
        param.push('(');
        for i in &self.parameters {
            param.push_str(&format!("{}, ", i))
        }
        param.push(')');
        write!(
            f,
            "fun {}{} {};",
            self.identifier.val, param, self.statement.val
        )
    }
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "return {};",
            self.return_value
                .clone()
                .unwrap_or(WithSpan::empty(Expression::Nil))
                .val
        )
    }
}
impl Display for GroupingExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for PropertyExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for ExtendsExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for ForExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for WhileExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for SuperExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for AssignExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for LogicalExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for ListExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for ClassStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
