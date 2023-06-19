# Scala Lox

This aims to be a tree-walk interpreter for the Lox language from 
[Crafting Interpreters](https://craftinginterpreters.com/) written in Scala 3.3.0.
The project uses the [Scala Build Tool](https://www.scala-sbt.org/) for compilation
and uses [ScalaTest](https://www.scalatest.org/) for testing.

We aim to produce well maintainable code that is well-documented, 
implements good design patterns and is (somewhat) tested.

## Differences
Compared to the tree-walk interpreter in the book, I'm attempting to do the challenges listed as well.
- Added multi-line comment support.  
- Added scanning and parsing for ternary operator.
- Added parsing for comma operator.
- Added custom errors for binary operators without left operand.