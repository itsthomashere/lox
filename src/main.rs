use lexer::Lexer;
use token::positions::LineOffset;

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
    let offsests = LineOffset::new(code);
    for i in result {
        println!("Line:{}.\t{}", offsests.line(i.clone().span.end), i)
    }
}
