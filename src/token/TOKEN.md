## Defining token

By definition we will have these token with their respective character in the program.

- Identifier 
    - IDENTIFIER
    - STRING
    - NUMBER
- Special keyword
    - FUNCTION: "fun"
    - LET: "let"
    - RETURN: "return"
    - IF: "if"
    - ELSE: "else"
    - WHILE: "while"
    - NIL: "nil"
- Delimiters
    - LEFTPAREN: "("
    - RIGHTPAREN: ")"
    - LEFTBRACE: "{"
    - RIGHTBRACE: "}"
    - LEFTBRACKET: "["
    - RIGHTBRACKET: "]"
    - COMMA: ","
    - SEMICOLON: ";"
    - COLON: ":"
    - DOT: "."
- Operators
    - PLUS: "+"
    - MINUS: "-"
    - ASTERISK: "*"
    - SLASH: "/"
    - BANG: "!"
    - ASSIGN: "="
    - LESS: "<"
    - LESSTHAN: "<="
    - GREATER: ">"
    - GREATERTHAN: ">="
    - EQUAL: "=="
    - NOTEQUAL: "!="
    - AND: "&&"
    - OR: "||"
- EOF and error
    - EOF
    - ILLEGAL
---
## Example

Source code is just a buch of token together, for example:
```rust
let a = b;
```

Can be seen as:
```
LET -> IDENTIFIER(a) -> ASSIGN -> IDENTIFIER(b) -> SEMICOLON -> EOF
```

