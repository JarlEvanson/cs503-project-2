package slox;

import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import scala.util.boundary
import scala.util.boundary.break

class Resolver(interpreter: Interpreter) extends ExprVisitor[Unit], StmtVisitor[Unit]:
  val scopes = Stack[Map[String, Boolean]]();
  var currentFunction = FunctionType.None;

  def resolve(stmts: Array[Stmt]): Unit = {
    for (stmt <- stmts) {
      resolve(stmt)
    }
  }

  def resolve(stmt: Stmt): Unit = stmt.accept(this);
  def resolve(expr: Expr): Unit = expr.accept(this);

  // Interesting Statements
  def visitBlock(stmt: BlockStmt): Unit = {
    beginScope();
    resolve(stmt.stmts);
    endScope();
  }

  def visitFunction(stmt: FunctionStmt): Unit = {
    declare(stmt.name);
    define(stmt.name);

    resolveFunction(stmt, FunctionType.Function);
  }

  def visitVar(stmt: VarStmt): Unit = {
    declare(stmt.name);
    if stmt.initializer != null then resolve(stmt.initializer);
    define(stmt.name);
  }

  // Interesting Expressions
  def visitVariable(expr: VariableExpr): Unit = {
    if scopes.length != 0 && scopes.top.get(expr.name.lexeme) == Some(false) then
      Lox.error(expr.name, "Can't read local variable in its own initializer.");
    resolveLocal(expr, expr.name);
  }

  def visitAssign(expr: AssignExpr): Unit = {
    resolve(expr.value);
    resolveLocal(expr, expr.name);
  }

  // Uninteresting Statements
  def visitExpression(stmt: ExpressionStmt): Unit = resolve(stmt.expr);
  def visitIf(stmt: IfStmt): Unit = {
    resolve(stmt.condition);
    resolve(stmt.thenBranch);
    if stmt.elseBranch != null then resolve(stmt.elseBranch);
  }
  def visitPrint(stmt: PrintStmt): Unit = resolve(stmt.expr);
  def visitReturn(stmt: ReturnStmt): Unit = {
    if currentFunction == FunctionType.None then
      Lox.error(stmt.keyword, "Can't return from top-level code.");
    if stmt.value != null then resolve(stmt.value);
  }
  def visitWhile(stmt: WhileStmt): Unit = {
    resolve(stmt.condition);
    resolve(stmt.body);
  }
  // Uninteresting Expressions
  def visitBinary(expr: BinaryExpr): Unit = {
    resolve(expr.left);
    resolve(expr.right);
  }
  def visitCall(expr: CallExpr): Unit = {
    resolve(expr.callee);
    for (arg <- expr.arguments) resolve(arg);
  }
  def visitGrouping(expr: GroupingExpr): Unit = resolve(expr.expr);
  def visitLiteral(expr: LiteralExpr): Unit = {}
  def visitLogical(expr: LogicalExpr): Unit = {
    resolve(expr.left);
    resolve(expr.right);
  }
  def visitUnary(expr: UnaryExpr): Unit = resolve(expr.right);

  def beginScope(): Unit = scopes.push(Map[String, Boolean]());
  def declare(name: Token): Unit = {
    if scopes.length == 0 then return;
    val scope = scopes.top;
    if scope.contains(name.lexeme) then
      Lox.error(name, "Already a variable with this name in this scope.");
    scope(name.lexeme) = false;
  }
  def define(name: Token): Unit = {
    if scopes.length == 0 then return;
    scopes.top(name.lexeme) = true;
  }
  def resolveLocal(expr: Expr, name: Token): Unit = {
    boundary:
      for ((scope, index) <- scopes.zipWithIndex) {
        if (scope.contains(name.lexeme)) {
          interpreter.resolve(expr, index);
          break()
        }
      }
  }
  def resolveFunction(function: FunctionStmt, kind: FunctionType): Unit = {
    beginScope();
    val enclosingFunction = currentFunction;
    currentFunction = kind;
    for (param <- function.params) {
      declare(param);
      define(param);
    }
    resolve(function.body);
    endScope();
    currentFunction = enclosingFunction;
  }
  def endScope(): Unit = scopes.pop();

enum FunctionType:
  case None, Function
