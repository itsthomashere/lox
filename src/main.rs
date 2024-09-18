use lexer::Lexer;

mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
        let x = 10;
    "#;
    let mut lexer = Lexer::new(code);
    let result = lexer.lex();
    for i in result {
        println!("{}", i)
    }
}
