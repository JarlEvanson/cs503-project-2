package slox;

class Interpreter extends StmtVisitor[Any] with ExprVisitor[Any]:
  def interpret(stmts: Array[Stmt]): Unit = {
    try
      for (stmt <- stmts) {
        execute(stmt)
      }
    catch
      case e: RuntimeError => Lox.runtimeError(e);
  }

  def execute(stmt: Stmt): Unit = stmt.accept(this)
  def evaluate(expr: Expr): Any = expr.accept(this)

  def visitExpression(stmt: ExpressionStmt): Any = {
    evaluate(stmt.expr);
    return null;
  }

  def visitPrint(stmt: PrintStmt): Any = {
    val value = evaluate(stmt.expr);
    println(stringify(value));
    return null;
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
  def visitGrouping(expr: GroupingExpr): Any = evaluate(expr.expr)
  def visitLiteral(expr: LiteralExpr): Any = expr.value
  def visitUnary(expr: UnaryExpr): Any = {
    val right = evaluate(expr.right);

    expr.operator.kind match
      case TokenType.Bang => !isTruthy(right)
      case TokenType.Minus => - right.asInstanceOf[Double]
      case _ => throw new RuntimeError(expr.operator, "unreachable")
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
