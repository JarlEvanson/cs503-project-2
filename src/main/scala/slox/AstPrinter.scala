package slox;

class AstPrinter extends ExprVisitor[String]:
  def printExpr(expr: Expr): String = expr.accept(this)

  def visitBinary(expr: BinaryExpr): String = parenthesize(expr.operator.lexeme, expr.left, expr.right);
  def visitGrouping(expr: GroupingExpr): String = parenthesize("group", expr.expr);
  def visitLiteral(expr: LiteralExpr): String = {
    if expr.value == null then return "nil";
    expr.value.toString()
  }
  def visitUnary(expr: UnaryExpr): String = parenthesize(expr.operator.lexeme, expr.right);


  def parenthesize(name: String, exprs: Expr*): String = {
    var builder = StringBuilder();

    builder.append("(").append(name);
    for (expr <- exprs) {
      builder.append(" ").append(expr.accept(this));
    }
    builder.append(")");

    builder.toString()
  }
