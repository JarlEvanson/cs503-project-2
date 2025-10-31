package slox;

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class Interpreter extends StmtVisitor[Any] with ExprVisitor[Any]:
  var globals = Environment();
  var environment = globals;
  var locals = Map[Expr, Int]();

  globals.define("clock", new LoxCallable {
    def arity(): Int = 0;
    def call(interpreter: Interpreter, args: Array[Any]): Any = {
      System.currentTimeMillis().toDouble / 1000.0
    }
    override def toString(): String = "<native fn>";
  });

  def interpret(stmts: Array[Stmt]): Unit = {
    try
      for (stmt <- stmts) {
        execute(stmt)
      }
    catch
      case e: RuntimeError => Lox.runtimeError(e);
  }

  def execute(stmt: Stmt): Unit = stmt.accept(this)
  def resolve(expr: Expr, depth: Int): Unit = locals.put(expr, depth);

  def executeBlock(stmts: Array[Stmt], env: Environment): Unit = {
    val previous = this.environment;
    try
      this.environment = env;

      for (stmt <- stmts) {
        execute(stmt);
      }
    finally
      this.environment = previous;
  }

  def visitBlock(stmt: BlockStmt): Any = {
    executeBlock(stmt.stmts, Environment(environment));
    return null;
  }

  def visitExpression(stmt: ExpressionStmt): Any = {
    evaluate(stmt.expr);
    return null;
  }

  def visitFunction(stmt: FunctionStmt): Any = {
    val function = LoxFunction(stmt, environment);
    environment.define(stmt.name.lexeme, function);
    return null;
  }

  def visitIf(stmt: IfStmt): Any = {
    if isTruthy(evaluate(stmt.condition)) then
      execute(stmt.thenBranch)
    else if stmt.elseBranch != null then
      execute(stmt.elseBranch)

    null
  }

  def visitPrint(stmt: PrintStmt): Any = {
    val value = evaluate(stmt.expr);
    println(stringify(value));
    return null;
  }

  def visitReturn(stmt: ReturnStmt): Any = {
    var value: Any = null;
    if stmt.value != null then value = evaluate(stmt.value);
    
    throw Return(value)
  }

  def visitVar(stmt: VarStmt): Any = {
    val value = stmt.initializer match
      case null => null
      case initializer => evaluate(initializer)

    environment.define(stmt.name.lexeme, value);
    return null;
  }

  def visitWhile(stmt: WhileStmt): Any = {
    while (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.body);
    }

    return null;
  }

  def evaluate(expr: Expr): Any = expr.accept(this)

  def visitAssign(expr: AssignExpr): Any = {
    val value = evaluate(expr.value);
    val distance = locals.get(expr);
    distance match
      case Some(distance) => environment.assignAt(distance, expr.name, value);
      case None => globals.assign(expr.name, value);

    value
  }

  def visitBinary(expr: BinaryExpr): Any = {
    val left = evaluate(expr.left);
    val right = evaluate(expr.right);

    expr.operator.kind match
      case TokenType.Minus => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] - right.asInstanceOf[Double]
      }
      case TokenType.Plus => {
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] + right.asInstanceOf[Double]
        else if left.isInstanceOf[String] && right.isInstanceOf[String] then
          left.asInstanceOf[String] + right.asInstanceOf[String]
        else
          throw new RuntimeError(
            expr.operator,
            "Operands must be two numbers or two strings."
          );
      }
      case TokenType.Slash => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] / right.asInstanceOf[Double]
      }
      case TokenType.Star => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] * right.asInstanceOf[Double]
      }
      case TokenType.Greater => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      }
      case TokenType.GreaterEqual => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      }
      case TokenType.Less => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      }
      case TokenType.LessEqual => {
        checkNumberOperands(expr.operator, left, right);
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      }
      case TokenType.BangEqual => {
        checkNumberOperands(expr.operator, left, right);
        !isEqual(left.asInstanceOf[Double], right.asInstanceOf[Double])
      }
      case TokenType.EqualEqual => {
        checkNumberOperands(expr.operator, left, right);
        isEqual(left.asInstanceOf[Double], right.asInstanceOf[Double])
      }
      case _ => throw new RuntimeError(expr.operator, "unreachable")
  }
  def visitCall(expr: CallExpr): Any = {
    val callee = evaluate(expr.callee);

    var args = ArrayBuffer[Any]();
    for (arg <- expr.arguments) {
      args += evaluate(arg);
    }
    
    if !callee.isInstanceOf[LoxCallable] then
      throw RuntimeError(expr.closingParen, "Can only call functions and classes");
    val function = callee.asInstanceOf[LoxCallable];
    if args.length != function.arity() then
      throw RuntimeError(
        expr.closingParen,
        "Expected " + function.arity() + " arguments but got " + args.length + "."
      );

    function.call(this, args.toArray);
  }
  def visitGrouping(expr: GroupingExpr): Any = evaluate(expr.expr)
  def visitLiteral(expr: LiteralExpr): Any = expr.value
  def visitLogical(expr: LogicalExpr): Any = {
    val left = evaluate(expr.left);

    if (expr.operator.kind == TokenType.Or) {
      if (isTruthy(left)) {
        return left;
      }
    } else {
      if (!isTruthy(left)) {
        return left;
      }
    }

    evaluate(expr.right)
  }
  def visitUnary(expr: UnaryExpr): Any = {
    val right = evaluate(expr.right);

    expr.operator.kind match
      case TokenType.Bang => !isTruthy(right)
      case TokenType.Minus => - right.asInstanceOf[Double]
      case _ => throw new RuntimeError(expr.operator, "unreachable")
  }
  def visitVariable(expr: VariableExpr): Any = lookupVariable(expr.name, expr);
  def lookupVariable(name: Token, expr: Expr): Any = {
    val distance = locals.get(expr);
    distance match
      case Some(distance) => environment.getAt(distance, name.lexeme)
      case None => globals.get(name)
  }

  def checkNumberOperand(operator: Token, operand: Any): Unit = {
    if operand.isInstanceOf[Double] then return;
    throw new RuntimeError(operator, "Operand must be a number.")
  }

  def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = {
    if left.isInstanceOf[Double] && right.isInstanceOf[Double] then return;
    throw new RuntimeError(operator, "Operands must be numbers.")
  }

  def isTruthy(obj: Any): Boolean = {
    if obj == null then return false;
    if obj.isInstanceOf[Boolean] then return obj.asInstanceOf[Boolean];
    true
  }

  def isEqual(a: Any, b: Any): Boolean = {
    if a == null && b == null then return true;
    if a == null then return false;

    a.equals(b)
  }

  def stringify(obj: Any): String = {
    if obj == null then return "nil";

    if obj.isInstanceOf[Double] then
      var text = obj.toString();
      if text.endsWith(".0") then text = text.substring(0, text.length() - 2);
      text
    else
      obj.toString()
  }
