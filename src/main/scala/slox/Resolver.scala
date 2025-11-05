package slox;

import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import scala.util.boundary
import scala.util.boundary.break

class Resolver(interpreter: Interpreter) extends ExprVisitor[Unit], StmtVisitor[Unit]:
  val scopes = Stack[Map[String, Boolean]]();
  var currentFunction = FunctionType.None;
  var currentClass = ClassType.None;

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

  def visitClass(stmt: ClassStmt): Unit = {
    val enclosingClass = currentClass;
    currentClass = ClassType.Class;

    declare(stmt.name);
    define(stmt.name);

    val selfInheritance = stmt.superclass != null
      && stmt.name.lexeme.equals(stmt.superclass.name.lexeme);
    if selfInheritance then Lox.error(stmt.superclass.name, "A class can't inherit from itself.");

    if (stmt.superclass != null) {
      currentClass = ClassType.Subclass;
      resolve(stmt.superclass);
    }

    if (stmt.superclass != null) {
      beginScope();
      scopes.top("super") = true;
    }

    beginScope();
    scopes.top("this") = true;

    for (method <- stmt.methods) {
      var declaration = FunctionType.Method;
      if method.name.lexeme.equals("init") then declaration = FunctionType.Initializer;
      resolveFunction(method, declaration);
    }

    endScope();
    
    if stmt.superclass != null then endScope();
    currentClass = enclosingClass;
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
    if (currentFunction == FunctionType.None) {
            Lox.error(stmt.keyword, "Can't return from top-level code.");
    }
    if (stmt.value != null) {
      if currentFunction == FunctionType.Initializer then
        Lox.error(stmt.keyword, "Can't return a value from an initializer.");

      resolve(stmt.value);
    }
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
  def visitGet(expr: GetExpr): Unit = resolve(expr.obj);
  def visitGrouping(expr: GroupingExpr): Unit = resolve(expr.expr);
  def visitLiteral(expr: LiteralExpr): Unit = {}
  def visitLogical(expr: LogicalExpr): Unit = {
    resolve(expr.left);
    resolve(expr.right);
  }
  def visitSet(expr: SetExpr): Unit = {
    resolve(expr.value);
    resolve(expr.obj);
  }
  def visitSuper(expr: SuperExpr): Unit = {
    if (currentClass == ClassType.None) {
      Lox.error(expr.keyword, "Can't use 'super' outside of a class.");
    } else if (currentClass != ClassType.Subclass) {
      Lox.error(expr.keyword, "Can't use 'super' in a class with no superclass.");
    }

    resolveLocal(expr, expr.keyword);
  }
  def visitThis(expr: ThisExpr): Unit = {
    if currentClass == ClassType.None then
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.");
    resolveLocal(expr, expr.keyword);
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
  case None, Function, Initializer, Method

enum ClassType:
  case None, Class, Subclass
