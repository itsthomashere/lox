# The lox programming language
The lox programming language features:
- Dynamic typing
- Automatic memory management
- Datatypes including
    - Boolean
    - Number: double precision floating point number
    - Strings
    - Nil
- Expressions
    - Arithmetic
    - Comparison
    - Logical Operator
    - Precedence and grouping
- Statements
- Variables
- Control flows
- Functions
- Closures
- Classes
    - Prototype 
    - Inheritance

# Examples
---------------
## Boolean

The boolean type is pretty straight forward, there are two boolean values, `true` and `false`
```rust
true; // not false
false; // not true
```
---
## Numbers
The type number settle for double-precision floating point, but we can still have integer and floats
```rust
100502; // A positive integer
-3090802; // A negative integer
1005.02; // A float
-3008.02; // A negative float
```
---

## Strings
String behave like we expected in any languages
```rust
"I am a string hehe";
""; // Empty string
"123"; // This is a string and not a number
```
---
## Nil

Nil is the value of "null"

---
## Expression
---
### Arithmetics

Arithmetic operations is the same as C-like languages.
```rust
add + me;
subtract - me;
multiply * me;
divide * me;
```
There's also a prefix one for negative
```rust
-negativeMe;
```
---
### Comparision
We have the standard comparison seen in language like rust
```rust
less < than;
lessThan <= orEqual;
greater > than;
greaterThan >= orEqual;
// Boolean comparison
1 == 2; // false
1 != 2; // true
123 == "abcd"; // false
123 == "123"; // false

```
---
### Logical 
We have the not `!`, and `&&`, or `||` operator.
```rust
!false; // true
!true; // false
true && true; // true
true && false; // false
true || false; // true
false || false; // false
```
---
### Precedence and grouping
We can do standard grouping of expression using `()`.

```rust
let  average = ( min + max ) * 2;
```
---
## Statements
Statemets dont evalutate to values, instead they are used to change the state of our program. For example:
```rust
print("Hello world");
"Some expression";
// We can also group statement using `{}`
{
    print("one statement");
    print("Two statement")
}
```

## Variables
You declare a variable using the `let` keyword. You can either initialize it right away, or omit it. If you omit it, the variable value is default to nil.

```rust
let newVariable = "abcd"; // "abcd"
let newVariable; // nil
```

## Control Flow
We have the GOAT trifecta: `if-else`, `while`, `for`.

```rust
// If statement
if (condition) {
    print("true")
} else {
    print("false")
}

let a = 1;
while (a < 10) {
    print(a);
    a = a + 1;
}

for (let a = 0; a < 10; a = a + 1) {
    print(a)
}

```
---
## Functions

Function call expression is the same as in Rust or C

```rust
createNew(a, b, c);
createNew();
```

Define your own function is straightforward using the `fun` keyword.
```rust
fun createNew(a, b) {
    print(a + b);
}

fun sum(a, b) {
    return a + b;
}
```
### Closures

Function are *first class*. Which mean you can get a reference to it, store it in value, pass around. For example:

```rust
fun addPairs(a, b) {
    return a + b;
}

fun println(a) {
    print(a);
}

println(addPairs(3,4)); // 7
```

When you combine them together, we can have this.

```rust
fun addSome(a) {
    let x = 0;

    fun add() {
        x = x + a;
        print(x)
    }

    return add;
}

let threw = addSome(3); // add
three(); // 3;
three(); // 6;
```
---

## Classes

Here's an example on how to use class in lox

```rust
class Cat {
    name;
    age;
    init(name, age) {
        this.name = name
        this.age = age
    }

    meow() {
        print("meow")
    }

    eat(x) {
        print(x)
    }
}
```

We can inherit from other class using the `<` operator. We can also override the inherited class initialization.

```rust
class Dog < Cat {
    drink;
    init(age, name, drink) {
        super(age, name);
        this.drink = drink;
    }
}
```
