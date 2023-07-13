# Scala Lox

This aims to be a tree-walk interpreter for the Lox language from 
[Crafting Interpreters](https://craftinginterpreters.com/) written in Scala 3.3.0.
The project uses the [Scala Build Tool](https://www.scala-sbt.org/) for compilation
and uses [ScalaTest](https://www.scalatest.org/) for testing.

We aim to produce maintainable code that is well-documented, 
implements good design patterns and is (somewhat) tested.

# Language Details
## Expression Evalulation
### Logical Or and And
These act as control flow operations since the implement short-circuiting.

The return value is **not** guaranteed to be the literal `true` or `false` but is only guaranteed
to have the same "truthiness" as what the boolean expression should evaluate to.

### Ternary Operator
Lox implements the ternary operator as a control flow structure as well. I.e. we only evaluate (and return)
the `?` term if the condition is true, else we evaluate and return the ':' term.
```agsl
var a = 0;
var b = a > 0 ? (a = 1) : (a = 2)
// a == 2, b == 2
```
## Variables and Scoping

Variables in Lox are dynamically typed. They are either of 
- 64-bit Floating Point Double
- String

We check for type errors during runtime.

Each block in Lox is specified as being a sequence of statements within a pair of braces `{ statements }`. 
Each block has its own lexical environment.

We *define* variables via `var x = 10;`. These declarations will be localised to the current block, so doing
```agsl
var a = 1;
{
  var a = a + 2;
  print a;
}
print a;
```
will print 3 followed by 1. Redefining a variable within a block merely assigns it. 

We *assign* variables via `x = 9;`. Assignments are expressions that return the value being assigned to
the variable. It is right associative so `a = b = c = 10` is treated as `a = (b = (c = 10))`, thus setting
each variable to 10.

A variable needs to be assigned before it is first evaluated.
Assignments modify the instance of the variable with the smallest scope that is possible. 
A `RuntimeError` occurs if we assign variables in a scope that they are not lexically available in.

We *evaluate* a variable when it is used for computation e.g. `print x+10`. This uses the instance of the variable
within the smallest possible scope or raises a `RuntimeError` if not available or not initialised.

## Control Flow
These work exactly like in C
### If-Else
```agsl
if (x == 0){  // condition: Expression
    y = 1;    // ifBody: Statement
}
else y = 2;   // elseBody: Statement
```
Note that to evaulate whether a condition is true, we evaluate the "truthiness" of the expression.
- `false` is false
- `nil` is false
- Everything else is true

### While Loops
```agsl
while (x < 10){  // condition: Expression
    x = x + 1    // body: Statement
}
``` 
Lox also supports `continue;` and `break;` statements within loops.

### For loops
```agsl
for (var i = 0; i < n; i = i + 1){
    if (i == 2) break;
}
```
For loops are implemented as syntactic sugar for while-loops.

## Functions
Functions are first class objects in Lox. We can do a "regular" function declaration as such:
```agsl
fun sum(a, b, c){
    var x = a + b + c;
    return x;
}
```
This is syntactic sugar for creating a function object and binding it to a name
```agsl
var sum = fun (a, b, c){
   var x = a + b + c;
   return x;
 }
```
These function objects are parsed as expressions and so can be used wherever we would normally use expressions.

Here are some examples:
```agsl
var a = (1 < 2 ? fun (a){return a*2;} : fun(a){return a/2;})(1);
// a == 2
```

```agsl
fun thrice(fn) {
    var x = 1; 
    for (var i = 1; i <= 3; i = i + 1) {
        x = fn(x);
    } 
    return x;
}
var a = thrice(fun (a) {return a*2;});
// a == 8
```

### Scoping
These functions are scoped with closures.
```agsl
fun makeCounter() {
  var i = 0;
  fun count(){
    i = i + 1;
    return i;
  }
  return count;
}
var counter = makeCounter();
var a = counter(); // 1
var b = counter(); // 2
```
This works since the `i` is captured by the closure of count.

## Classes
Lox supports classes with constructors, methods and getters. Fields are
set dynamically at runtime. There is currently no support for static functions
or fields.
```
class Circle {
  init(radius) { // constructor
    this.radius = radius;
  }

  area { // getter
    return 3.141592653 * this.radius * this.radius;
  }
  
  scale(factor){ // method
    this.radius = this.radius * factor;
  }
}

var circle = Circle(2);
circle.scale(2);
print circle.area; // Prints roughly "50.2655".
```
## Grammar
Will be filled in once everything is much more finalised.

# Implementation Details
## Differences from Book Version
Compared to the tree-walk interpreter in the book, I'm attempting to do the challenges listed as well.
- Added multi-line comment support.  
- Added ternary operator.
  - `a ? b : c` evaluates `a`, `b` and `c`. Returns `b` if `a` is truthy else `c`.
  - Higher precedence than equality (`==` and `!-`).
  - Right associative.
- Added comma operator.
  - `a,b,c` evaluates `a`, `b` and `c`. Returns the value of `c`.
  - Higher precedence than ternary operator.
- Added custom errors for binary operators without left operand.
- Added error on using variables without initialisation.
- Added support for printing expressions from the REPL.
- Added continue and break statements for usage within loops.
- Added lambda functions.
  - Replaced regular functions with syntactic sugar for lambdas.
- Added getters for classes
  - These are kind of like syntactic sugar for 0 arity functions
  - They are still shadowed by fields of the same name