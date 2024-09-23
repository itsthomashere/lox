use lexer::Lexer;
use parser::Parser;
mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
            for (let x = 0; x < 10; x = x + 1) {
                add(x);
            };
        "#;
    let mut lexer = Lexer::new(code);
    let lexer_res = lexer.lex();
    for i in lexer_res {
        println!("{}", i)
    }
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
