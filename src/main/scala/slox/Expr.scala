package slox;

sealed abstract class Expr:
  def accept[R](visitor: ExprVisitor[R]): R;

class BinaryExpr(val left: Expr, val operator: Token, val right: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitBinary(this);

class GroupingExpr(val expr: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitGrouping(this);

class LiteralExpr(val value: Any) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitLiteral(this);

class UnaryExpr(val operator: Token, val right: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitUnary(this);

trait ExprVisitor[R]:
  def visitBinary(expr: BinaryExpr): R;
  def visitGrouping(expr: GroupingExpr): R;
  def visitLiteral(expr: LiteralExpr): R;
  def visitUnary(expr: UnaryExpr): R;

sealed abstract class Stmt:
  def accept[R](visitor: StmtVisitor[R]): R;

class ExpressionStmt(val expr: Expr) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitExpression(this);

class PrintStmt(val expr: Expr) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitPrint(this);

trait StmtVisitor[R]:
  def visitExpression(stmt: ExpressionStmt): R;
  def visitPrint(stmt: PrintStmt): R;
