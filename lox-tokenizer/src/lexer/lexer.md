## What is Lexing

Lexing or  ***Lexical Analysis*** is the process in which you turn source code into set of token that other part of the interpreter can understands.
For example the following source code
```rust
let x = 1;
```
will be turn into set of token like this
```
Let -> Identifier("x") -> Assign -> Number(1.0);
```


