## What is parsing
Parsing is the process in which your interpreter turn a sequence of tokens into a tree representation.
There are a may approaches to this tasks, but they mainly broken down into two categories:
- Using a DSL
- Hand writing a parser
Pratt parsing is one of the most frequently used technique for hand writing a parser.

## Pratt Parsing
### Precedence climbing technique
In the precedence climbing technique, each type of token has their own precedence level. When we parse an expression, we'll be climbing from the lowest precedence to the highest possible in the expression.
This technique is quite effiencent since we can just left the operations as long as we find operators with the same precedence level. We would have to save a temporary result to evaluate higher precedence operators.
Let's take a look at an example: `2 + 3 * 4 + 5 == 19` . Idealy that will be turn somewhat into this: `(((2 + (3 * 4)) + 5) == 19)`. We will give equality operator precedence value 0, 1 for additive expression, 2 for multiplicative expressions.

- Steps : parse_expr(token = 2, precedence = 0)
    - Next token is +, precedence value 1 > 0;
    - Advance the token until found a token with higher precedence than +
    - Recursively call parse_exp until all operators are matched, or <eof>
