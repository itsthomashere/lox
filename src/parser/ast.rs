use crate::token::positions::WithSpan;

pub type Identifier = WithSpan<String>;

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Box<WithSpan<Expression>>),
    Block(BlockStatement),
    Function(FunctionStatement),
    Class(ClassStatement),
    Property,
}

#[derive(Debug, Clone)]
pub enum Expression {
    // Literal Expression
    Identifier(WithSpan<String>),
    Number(WithSpan<f64>),
    String(WithSpan<String>),
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
    Assign(AssignExpression),
    // Call and indexing
    Call(CallExpression),
    Index(IndexExpression),
    Extends(ExtendsExpression),
}

#[derive(Debug, Clone)]
pub struct ClassStatement {
    pub identifier: WithSpan<Identifier>,
    pub extends: Option<WithSpan<Expression>>,
    pub body: Box<WithSpan<Statement>>,
}

#[derive(Debug, Clone)]
pub struct FunctionStatement {
    pub identifier: WithSpan<Identifier>,
    pub parameters: Vec<WithSpan<Identifier>>,
    pub statement: Box<WithSpan<Statement>>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<WithSpan<Statement>>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Box<WithSpan<Expression>>,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub identifier: Identifier,
    pub expression: Box<WithSpan<Expression>>,
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
#[derive(Debug, Clone)]
pub struct ExtendsExpression {
    pub identifier: WithSpan<Identifier>,
}

/// [TODO:description]
///
/// * `condition`: [TODO:parameter]
/// * `consequence`: [TODO:parameter]
#[derive(Debug, Clone)]
pub struct ForExpression {
    pub condition: Box<WithSpan<Expression>>,
    pub consequence: Box<WithSpan<Statement>>,
}

/// [TODO:description]
///
/// * `condition`: [TODO:parameter]
/// * `consequence`: [TODO:parameter]
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct SuperExpression {
    pub arguments: Vec<WithSpan<Expression>>,
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct AssignExpression {
    pub identifier: Box<WithSpan<Identifier>>,
    pub expression: Box<WithSpan<Expression>>,
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub left: Box<WithSpan<Expression>>,
    pub operator: Box<WithSpan<LogicalOperator>>,
    pub right: Box<WithSpan<Expression>>,
}

#[derive(Debug)]
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
    pub operator: Box<WithSpan<BinaryOperator>>,
    pub right: Box<WithSpan<Expression>>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    And, // &&
    Or,  // ||
}

#[derive(Debug, Clone)]
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
