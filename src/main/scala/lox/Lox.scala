package lox

import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
object Lox {
  def main(args: Array[String]): Unit = {
    if args.length > 1 then
      println("Usage: scalalox [script]")
      System.exit(64)
    else if args.length == 1 then
      runFile(args(0))
    else
      runPrompt()
  }

  private def runFile(path: String): Unit = {
    val inputFile = Source.fromFile(path)
    run(inputFile.getLines().mkString("\n")) // re-insert the new line characters
    inputFile.close()
    if hadError then System.exit(65)
    if hadRuntimeError then System.exit(70)
  }

  private def runPrompt(): Unit = {
    var line: String = ""
    while (line != null){
      print("> ")
      line = readLine()
      if line != null then
        run(line)
        hadError = false
    }
  }

  private val interpreter = new Interpreter()
  private var hadError: Boolean = false
  private var hadRuntimeError: Boolean = false
  private def run(source: String): Unit = {
    val scanner: Scanner = Scanner(source)
    val tokens: ArrayBuffer[Token] = scanner.scanTokens()

    val parser: Parser = Parser(tokens)
    val statements: ArrayBuffer[Stmt] = parser.parse()

    if !hadError then interpreter.interpret(statements)
  }

  def runtimeError(error: RuntimeError): Unit = {
    System.err.println(error.getMessage + "\n[line " + error.token.line + "]")
    hadRuntimeError = true
  }
  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }
  def error(token: Token, message: String): Unit = {
    if token.tokenType == TokenType.EOF then report(token.line, " at end", message)
    else report(token.line, " at '" + token.lexeme + "'", message)
  }
  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("Line [" + line.toString + "] Error" + where + ": " + message)
    hadError = true
  }

}
