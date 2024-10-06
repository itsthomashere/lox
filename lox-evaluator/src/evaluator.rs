use lox_tokenizer::parser::ast::Statement;

pub fn evaluate(statement: Statement) -> String {
    match statement {
        Statement::Let(let_statement) => todo!(),
        Statement::Return(return_statement) => todo!(),
        Statement::Expression(expr_statement) => todo!(),
        Statement::Block(block_statement) => todo!(),
        Statement::Function(function_statement) => todo!(),
        Statement::Class(class_statement) => todo!(),
    }
}
