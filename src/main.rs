use lexer::Lexer;
use parser::Parser;
mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
        let x = 10;
        let y = 10.05;
        let thomas; 

        fun thomas(a, b) {
            let a = 3;
            let b = 4;

            return a * b;
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
