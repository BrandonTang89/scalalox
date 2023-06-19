# Scala Lox

This aims to be a tree-walk interpreter for the Lox language from 
[Crafting Interpreters](https://craftinginterpreters.com/) written in Scala 3.3.0.
The project uses the [Scala Build Tool](https://www.scala-sbt.org/) for compilation
and uses [ScalaTest](https://www.scalatest.org/) for testing.

We aim to produce well maintainable code that is well-documented, 
implements good design patterns and is (somewhat) tested.

## Precedence and Associativity
1. Expression
2. Ternary Operator
3. Equality
4. Comparison
5. Term
6. Factor
7. Unary
8. Primary

So far everything I've made seems to be left-associative.

## Differences
Compared to the tree-walk interpreter in the book, I'm attempting to do the challenges listed as well.
- Added multi-line comment support.  
- Added ternary operator.
  - `a ? b : c` evaluates `a`, `b` and `c`. Returns `b` if `a` is truthy else `c`.
  - Higher precedence than equality (`==` and `!-`)
- Added comma operator.
  - `a,b,c` evaluates `a`, `b` and `c`. Returns the value of `c`.
  - Higher precedence than ternary operator
- Added custom errors for binary operators without left operand.