package lox

import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
object Lox {
  def main(args: Array[String]): Unit = {
    if (args.length > 1) then
      println("Usage: scalalox [script]")
      System.exit(64)
    else if (args.length == 1) then
      runFile(args(0))
    else
      runPrompt()
  }

  private def runFile(path: String): Unit = {
    val inputFile = Source.fromFile(path)
    run(inputFile.getLines().mkString)
    inputFile.close()
    if hadError then System.exit(64)
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

  private var hadError: Boolean = false
  private def run(source: String): Unit = {
    val scanner: Scanner = Scanner(source)
    val tokens: ArrayBuffer[Token] = scanner.scanTokens()

    for (token <- tokens) do
      println(token)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }
  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("Line [" + line.toString + "] Error " + where + ": " + message)
    hadError = true
  }

}
