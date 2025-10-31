package slox;

class AstPrinter extends ExprVisitor[String]:
  def printExpr(expr: Expr): String = expr.accept(this)

  def visitAssign(expr: AssignExpr): String = "(assign " + expr.name + " " + expr.value + ")";
  def visitBinary(expr: BinaryExpr): String = parenthesize(expr.operator.lexeme, expr.left, expr.right);
  def visitGrouping(expr: GroupingExpr): String = parenthesize("group", expr.expr);
  def visitLiteral(expr: LiteralExpr): String = {
    if expr.value == null then return "nil";
    expr.value.toString()
  }
  def visitLogical(expr: LogicalExpr): String = parenthesize(expr.operator.lexeme, expr.left, expr.right);
  def visitUnary(expr: UnaryExpr): String = parenthesize(expr.operator.lexeme, expr.right);
  def visitVariable(expr: VariableExpr): String = "(lookup " + expr.name + ")";


  def parenthesize(name: String, exprs: Expr*): String = {
    var builder = StringBuilder();

    builder.append("(").append(name);
    for (expr <- exprs) {
      builder.append(" ").append(expr.accept(this));
    }
    builder.append(")");

    builder.toString()
  }
