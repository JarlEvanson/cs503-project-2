package slox;

import scala.collection.mutable.ArrayBuffer

import slox.Lox.report

class Parser(tokens: Array[Token]):
  var current = 0;

  def parse(): Array[Stmt] = {
    var statements = ArrayBuffer[Stmt]();
    while (!isAtEnd()) {
      statements += statement();
    }

    statements.toArray
  }

  def expression(): Expr = equality();
  
  def statement(): Stmt = {
    if matches(TokenType.Print) then
      printStatement() 
    else
      expressionStatement()
  }

  def printStatement(): Stmt = {
    val value = expression();
    consume(TokenType.Semicolon, "Expected ';' after value.");
    PrintStmt(value)
  }

  def expressionStatement(): Stmt = {
    val expr = expression();
    consume(TokenType.Semicolon, "Expect ';' after expression.");
    ExpressionStmt(expr)
  }

  def equality(): Expr = {
    var expr = comparision();

    while (matches(TokenType.BangEqual, TokenType.EqualEqual)) {
      val operator = previous();
      val right = comparision();
      expr = BinaryExpr(expr, operator, right);
    }

    expr
  }

  def comparision(): Expr = {
    var expr = term();

    val valid_tokens = Array(
      TokenType.Greater,
      TokenType.GreaterEqual,
      TokenType.Less,
      TokenType.LessEqual
    );
    while (matches(valid_tokens*)) {
      val operator = previous();
      val right = term();
      expr = BinaryExpr(expr, operator, right);
    }

    expr
  }

  def term(): Expr = {
    var expr = factor();

    while (matches(TokenType.Plus, TokenType.Minus)) {
      val operator = previous();
      val right = factor();
      expr = BinaryExpr(expr, operator, right);
    }

    expr
  }

  def factor(): Expr = {
    var expr = unary();

    while (matches(TokenType.Slash, TokenType.Star)) {
      val operator = previous();
      val right = unary();
      expr = BinaryExpr(expr, operator, right);
    }

    expr
  }

  def unary(): Expr = {
    if (matches(TokenType.Bang, TokenType.Minus)) {
      val operator = previous();
      val right = unary();
      return UnaryExpr(operator, right);
    }

    primary()
  }

  def primary(): Expr = {
    if (matches(TokenType.False)) return LiteralExpr(false);
    if (matches(TokenType.True)) return LiteralExpr(true);
    if (matches(TokenType.Nil)) return LiteralExpr(null);

    if (matches(TokenType.Number, TokenType.String)) then
      return LiteralExpr(previous().literal);

    if (matches(TokenType.LeftParen)) {
      val expr = expression();
      consume(TokenType.RightParen, "Expected ')' after expression.");
      return GroupingExpr(expr);
    }

    throw error(peek(), "Expect expression.")
  }

  def matches(kinds: TokenType*): Boolean = {
    var found = false;
    for (kind <- kinds) {
      if (check(kind) && !found) {
        advance();
        found = true;
      }
    }

    found
  }

  def consume(kind: TokenType, message: String): Token = {
    if check(kind) then return advance();

    throw error(peek(), message)
  }

  def check(kind: TokenType): Boolean = !isAtEnd() && peek().kind == kind
  def advance(): Token = {
    if !isAtEnd() then current += 1;
    previous()
  }

  def isAtEnd(): Boolean = peek().kind == TokenType.Eof
  def peek(): Token = tokens(current)
  def previous(): Token = tokens(current - 1)

  def error(token: Token, message: String): ParseError = {
    slox.Lox.error(token, message);
    ParseError()
  }

  def synchronize(): Unit = {
    advance();

    while (!isAtEnd()) {
      if (previous().kind == TokenType.Semicolon) return;

      peek().kind match
        case TokenType.Class | TokenType.Fun | TokenType.Var
          | TokenType.For | TokenType.If | TokenType.While
          | TokenType.Print | TokenType.Return => return;
        case _ => {}

      advance();
    }
  }

class ParseError extends Throwable
