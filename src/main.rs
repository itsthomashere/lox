use lexer::Lexer;

mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
        let x = 10;
        let y = 10.05;
        let thomas = "String";
    "#;
    let mut lexer = Lexer::new(code);
    let result = lexer.lex_with_context();
    for i in result {
        println!("{}", i)
    }
}
