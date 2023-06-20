# Scala Lox

This aims to be a tree-walk interpreter for the Lox language from 
[Crafting Interpreters](https://craftinginterpreters.com/) written in Scala 3.3.0.
The project uses the [Scala Build Tool](https://www.scala-sbt.org/) for compilation
and uses [ScalaTest](https://www.scalatest.org/) for testing.

We aim to produce maintainable code that is well-documented, 
implements good design patterns and is (somewhat) tested.

# Language Details
## Variables and Scoping

Variables in Lox are dynamically typed. They are either of 
- 64-bit Floating Point Double
- String

We check for type errors during runtime.

Each block in Lox is specified as being a sequence of statements within a pair of braces `{ statements }`. 
Each block has its own lexical environment.

We *define* variables via `var x = 10;`. These declarations will be localised to the current block, so doing
```lox
var a = 1;
{
  var a = a + 2;
  print a;
}
print a;
```
will print 3 followed by 1.

We *assign* variables via `x = 9;`. Assignments are expressions that return the value being assigned to
the variable. It is right associative so `a = b = c = 10` is treated as `a = (b = (c = 10))`, thus setting
each variable to 10.

A variable needs to be assigned before it is first evaluated.
Assignments modify the instance of the variable with the smallest scope that is possible. 
A `RuntimeError` occurs if we assign variables in a scope that they are not lexically available in.

We *evaluate* a variable when it is used for computation e.g. `print x+10`. This uses the instance of the variable
within the smallest possible scope or raises a `RuntimeError` if not available or not initialised.
## Grammar
Will be filled in once everything is much more finalised.

# Implementation Details
## Differences from Book Version
Compared to the tree-walk interpreter in the book, I'm attempting to do the challenges listed as well.
- Added multi-line comment support.  
- Added ternary operator.
  - `a ? b : c` evaluates `a`, `b` and `c`. Returns `b` if `a` is truthy else `c`.
  - Higher precedence than equality (`==` and `!-`)
  - Right associative
- Added comma operator.
  - `a,b,c` evaluates `a`, `b` and `c`. Returns the value of `c`.
  - Higher precedence than ternary operator
- Added custom errors for binary operators without left operand.
- Added error on using variables without initialisation
- Added support for printing expressions from the REPL