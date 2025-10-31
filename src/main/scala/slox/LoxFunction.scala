package slox;

class LoxFunction(declaration: FunctionStmt, closure: Environment) extends LoxCallable:
  def arity(): Int = declaration.params.length
  def call(interpreter: Interpreter, args: Array[Any]): Any = {
    var env = Environment(closure);
    for ((name, arg) <- declaration.params.zip(args)) {
      env.define(name.lexeme, arg);
    }

    try
      interpreter.executeBlock(declaration.body, env);
    catch
      case e: Return => return e.value
    return null;
  }

  override def toString(): String = "<fn " + declaration.name.lexeme + ">";
