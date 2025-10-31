package slox;

sealed abstract class Expr:
  def accept[R](visitor: ExprVisitor[R]): R;

class AssignExpr(val name: Token, val value: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitAssign(this);

class BinaryExpr(val left: Expr, val operator: Token, val right: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitBinary(this);

class GroupingExpr(val expr: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitGrouping(this);

class LiteralExpr(val value: Any) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitLiteral(this);

class LogicalExpr(val left: Expr, val operator: Token, val right: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitLogical(this);

class UnaryExpr(val operator: Token, val right: Expr) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitUnary(this);

class VariableExpr(val name: Token) extends Expr:
  def accept[R](visitor: ExprVisitor[R]): R = visitor.visitVariable(this);

trait ExprVisitor[R]:
  def visitAssign(expr: AssignExpr): R;
  def visitBinary(expr: BinaryExpr): R;
  def visitGrouping(expr: GroupingExpr): R;
  def visitLiteral(expr: LiteralExpr): R;
  def visitLogical(expr: LogicalExpr): R;
  def visitUnary(expr: UnaryExpr): R;
  def visitVariable(expr: VariableExpr): R;

sealed abstract class Stmt:
  def accept[R](visitor: StmtVisitor[R]): R;

class BlockStmt(val stmts: Array[Stmt]) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitBlock(this);

class IfStmt(val condition: Expr, val thenBranch: Stmt, val elseBranch: Stmt) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitIf(this);

class ExpressionStmt(val expr: Expr) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitExpression(this);

class PrintStmt(val expr: Expr) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitPrint(this);

class VarStmt(val name: Token, val initializer: Expr) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitVar(this);

class WhileStmt(val condition: Expr, val body: Stmt) extends Stmt:
  def accept[R](visitor: StmtVisitor[R]): R = visitor.visitWhile(this);

trait StmtVisitor[R]:
  def visitBlock(stmt: BlockStmt): R;
  def visitIf(stmt: IfStmt): R;
  def visitExpression(stmt: ExpressionStmt): R;
  def visitPrint(stmt: PrintStmt): R;
  def visitVar(stmt: VarStmt): R;
  def visitWhile(stmt: WhileStmt): R;
