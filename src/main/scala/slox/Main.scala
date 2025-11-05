package slox;

import scala.io.Source
import scala.io.StdIn
import scala.util.control.Breaks._

  @main def main(args: String*): Unit = {
    if (args.length > 1)
      println("Usage: jlox [script]");
      System.exit(64);
    else if (args.length == 1)
      Lox.runFile(args(0));
    else
      Lox.runPrompt();
  }

object Lox:
  var hadError = false;
  var hadRuntimeError = false;
  var interpreter = Interpreter();

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path, "utf-8");
    val string = try source.mkString finally source.close();
    run(string)

    // Indicate an error occurred in the exit code.
    if (hadError) System.exit(65);
    if (hadRuntimeError) System.exit(70);
  }

  def runPrompt(): Unit = {
    breakable {
      while (true) {
        val line = StdIn.readLine("> ");
        if (line == null) break
        run(line)
        hadError = false;
      }
    }
  }

  def run(source: String): Unit = {
    var scanner = Scanner(source);
    val tokens = scanner.scanTokens();
    var parser = Parser(tokens);
    val stmts = parser.parse();

    // Stop if there was a syntax error.
    if (hadError) return;

    var resolver = Resolver(interpreter);
    resolver.resolve(stmts);

    // Stop if there was a resolution error.
    if (hadError) return;
    
    interpreter.interpret(stmts);
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message);
  }

  def report(line: Int, where: String, message: String): Unit = {
    Console.err.println("[line " + line + "] Error" + where + ": " + message);
    hadError = true;
  }

  def error(token: Token, message: String): Unit = {
    if (token.kind == TokenType.Eof) then
      report(token.line, " at end", message);
    else
      report(token.line, " at '" + token.lexeme + "'", message);
  }

  def runtimeError(error: RuntimeError): Unit = {
    Console.err.println(
      error.getMessage() + "\n[line " + error.token.line + "]");
    hadRuntimeError = true;
  }
