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
## An ast
Your program consists of statement and expressions, Expressions are some code that evaluates to values like `10 + 4`, `a + b`. Statements on the other hand doesn't evaluate to values, but can internally change the state of your program. Statements usually consists of one or multiple expressions.
### Expression
Generally speaking, expressions can be one of these three:
- A postfix expression: Generally have this structure: `<operator><expression>`
    - Example: `a++, a--, b**`
    - Lox doesn't have real postfix expression
- An infix expression: Generally have this structure: `<expression> <operator> <expression>`
    - Example: `10 + 2, 10 <= 2, 10 = 2`
    - Lox have these type of infix expression
        - Logical Expression: `10 + 2`
        - Assign Expression: `a = b`
- A prefix expression: Generally have this structure: `<operator><expression>`
    - Example: `-2, +a, !true`
    - Lox have these type of prefix expression
        - Unary Expression
If an expression doesn't fall into those 3 categories, it will needs to have an associated type of expression for example IfExpression( if it can return value)
### Statement


