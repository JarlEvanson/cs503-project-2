package slox;

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

import slox.Lox.report

class Parser(tokens: Array[Token]):
  var current = 0;

  def parse(): Array[Stmt] = {
    var statements = ArrayBuffer[Stmt]();
    while (!isAtEnd()) {
      statements += declaration();
    }

    statements.toArray
  }

  def expression(): Expr = assignment();
  
  def declaration(): Stmt = {
    try
      if matches(TokenType.Class) then return classDeclaration();
      if matches(TokenType.Fun) then return function("function");
      if matches(TokenType.Var) then return varDeclaration();

      statement()
    catch
      case e: ParseError => {
        synchronize();
        return null;
      }
  }

  def classDeclaration(): Stmt = {
    val name = consume(TokenType.Identifier, "Expect class name.");

    var superclass: VariableExpr = null;
    if (matches(TokenType.Less)) {
      consume(TokenType.Identifier, "Expect superclass name.");
      superclass = VariableExpr(previous());
    }

    consume(TokenType.LeftBrace, "Expect '{' before class body.");

    var methods = ArrayBuffer[FunctionStmt]();
    while (!check(TokenType.RightBrace) && !isAtEnd()) {
      methods += function("method");
    }

    consume(TokenType.RightBrace, "Expect '}' after class body.");
    ClassStmt(name, superclass, methods.toArray)
  }

  def statement(): Stmt = {
    if matches(TokenType.Print) then
      printStatement() 
    else if matches(TokenType.LeftBrace) then
      BlockStmt(block())
    else if matches(TokenType.If) then
      ifStatement()
    else if matches(TokenType.While) then
      whileStatement()
    else if matches(TokenType.For) then
      forStatement()
    else if matches(TokenType.Return) then
      returnStatement()
    else
      expressionStatement()
  }

  def forStatement(): Stmt = {
    consume(TokenType.LeftParen, "Expect '(' after 'for'.");

    var initializer: Stmt = null;
    if matches(TokenType.Semicolon) then
      initializer = null;
    else if matches(TokenType.Var) then
      initializer = varDeclaration();
    else
      initializer = expressionStatement();

    var condition: Expr = null;
    if !check(TokenType.Semicolon) then condition = expression();
    consume(TokenType.Semicolon, "Expect ';' after loop condition.");

    var increment: Expr = null;
    if !check(TokenType.RightParen) then increment = expression();
    consume(TokenType.RightParen, "Expect ')' after for clauses.");

    var body = statement();

    if increment != null then body = BlockStmt(Array(body, ExpressionStmt(increment)));
    if condition == null then condition = LiteralExpr(true);
    body = WhileStmt(condition, body);

    if initializer != null then body = BlockStmt(Array(initializer, body));

    body
  }

  def whileStatement(): Stmt = {
    consume(TokenType.LeftParen, "Expect '(' after 'while'.");
    val condition = expression();
    consume(TokenType.RightParen, "Expect ')' after condition.");

    val body = statement();
    WhileStmt(condition, body)
  }

  def ifStatement(): Stmt = {
    consume(TokenType.LeftParen, "Expect '(' after 'if'.");
    val condition = expression();
    consume(TokenType.RightParen, "Expect ')' after if condition.");

    val thenBranch = statement();
    val elseBranch = matches(TokenType.Else) match
      case true => statement()
      case false => null

    IfStmt(condition, thenBranch, elseBranch)
  }

  def printStatement(): Stmt = {
    val value = expression();
    consume(TokenType.Semicolon, "Expected ';' after value.");
    PrintStmt(value)
  }

  def returnStatement(): Stmt = {
    val keyword = previous();
    var value: Expr = null;
    if !check(TokenType.Semicolon) then value = expression();

    consume(TokenType.Semicolon, "Expect ';' after return value.");
    ReturnStmt(keyword, value)
  }

  def varDeclaration(): Stmt = {
    val name = consume(TokenType.Identifier, "Expect variable name.");

    val initializer = matches(TokenType.Equal) match
      case true => expression()
      case false => null

    consume(TokenType.Semicolon, "Expect ';' after variable declaration.");
    VarStmt(name, initializer)
  }

  def expressionStatement(): Stmt = {
    val expr = expression();
    consume(TokenType.Semicolon, "Expect ';' after expression.");
    ExpressionStmt(expr)
  }

  def function(kind: String): FunctionStmt = {
    val name = consume(TokenType.Identifier, "Expect " + kind + " name.");
    consume(TokenType.LeftParen, "Expect '(' after " + kind + " name.");

    var params = ArrayBuffer[Token]();
    if (!check(TokenType.RightParen)) then
      while
        if params.length >= 255 then error(peek(), "Can't have more than 255 parameters.");
        params += consume(TokenType.Identifier, "Expect parameter name.");
        matches(TokenType.Comma)
      do ()
    consume(TokenType.RightParen, "Expect ')' after parameters.");

    consume(TokenType.LeftBrace, "Expect '{' before " + kind + " body.");
    val body = block();
    FunctionStmt(name, params.toArray, body);
  }

  def block(): Array[Stmt] = {
    var stmts = ArrayBuffer[Stmt]();

    while (!check(TokenType.RightBrace) && !isAtEnd()) {
      stmts += declaration();
    }

    consume(TokenType.RightBrace, "Expect '}' after block.");
    stmts.toArray
  }

  def assignment(): Expr = {
    val expr = or();

    if matches(TokenType.Equal) then
      val equals = previous();
      val value = assignment();

      if expr.isInstanceOf[VariableExpr] then
        val name = expr.asInstanceOf[VariableExpr].name;
        return AssignExpr(name, value)
      else if expr.isInstanceOf[GetExpr] then
        val get = expr.asInstanceOf[GetExpr];
        return SetExpr(get.obj, get.name, value);


      error(equals, "Invalid assignment target.");

    return expr;
  }

  def or(): Expr = {
    var expr = and();

    while (matches(TokenType.Or)) {
      val operator = previous();
      val right = and();
      expr = LogicalExpr(expr, operator, right);
    }

    expr
  }

  def and(): Expr = {
    var expr = equality();

    while (matches(TokenType.And)) {
      val operator = previous();
      val right = equality();
      expr = LogicalExpr(expr, operator, right);
    }

    expr
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

    call()
  }

  def call(): Expr = {
    var expr = primary();

    breakable {
      while (true) {
        if (matches(TokenType.LeftParen)) {
          expr = finishCall(expr);
        } else if (matches(TokenType.Dot)) {
          val name = consume(TokenType.Identifier, "Expect property name after '.'.");
          expr = GetExpr(expr, name)
        } else {
          break
        }
      }
    }

    expr
  }

  def finishCall(callee: Expr): Expr = {
    var args = ArrayBuffer[Expr]();
    if (!check(TokenType.RightParen)) {
      while
        if args.length >= 255 then error(peek(), "Can't have more than 255 arguments.");
        args += expression();
        matches(TokenType.Comma)
      do ()
    }

    val paren = consume(TokenType.RightParen, "Expect ')' after arguments.");
    CallExpr(callee, args.toArray, paren)
  }

  def primary(): Expr = {
    if (matches(TokenType.False)) return LiteralExpr(false);
    if (matches(TokenType.True)) return LiteralExpr(true);
    if (matches(TokenType.Nil)) return LiteralExpr(null);

    if (matches(TokenType.Number, TokenType.String)) then
      return LiteralExpr(previous().literal);

    if matches(TokenType.This) then
      return ThisExpr(previous());

    if (matches(TokenType.Super)) {
      val keyword = previous();
      consume(TokenType.Dot, "Expect '.' after 'super'.");
      val method = consume(TokenType.Identifier, "Expect superclass method name.");
      return SuperExpr(keyword, method);
    }

    if matches(TokenType.Identifier) then
      return VariableExpr(previous());

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
