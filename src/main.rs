use lexer::Lexer;
use parser::Parser;
mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
        if (x < 10) {
            return 2;
        } else {
            return "thomas";
        }; 
        "#;
    let lexer = Lexer::new(code);
    let mut parser = Parser::from_lexer(lexer);
    let program = parser.parse_program();
    let errors = parser.get_errors();
    for i in errors {
        println!("{:#?}", i);
    }
    for statement in program {
        println!("{:#?}", statement);
    }
}
