use lexer::Lexer;
use parser::Parser;
mod lexer;
mod parser;
mod token;

fn main() {
    let code = r#"
        fun add(a, b, c, d) {
            return (a + b) * (c + d);
        };
        add(1,2,3,4);

        let identifier = (a);

        for (let x = 0; x < 10; x = x + 1) {
            add(x,2,3,4);
        };

        while (x < 10) {
            add(x);
        };
    "#;
    let lexer = Lexer::new(code);
    let mut parser = Parser::from_lexer(lexer);
    let program = parser.parse_program();
    let errors = parser.get_errors();
    for i in errors {
        println!("{:?}", i);
    }
    for statement in program {
        println!("{:?}", statement);
    }
}
