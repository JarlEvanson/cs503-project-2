package slox;

class LoxFunction(declaration: FunctionStmt, closure: Environment, isInitializer: Boolean) extends LoxCallable:
  def arity(): Int = declaration.params.length
  def call(interpreter: Interpreter, args: Array[Any]): Any = {
    var env = Environment(closure);
    for ((name, arg) <- declaration.params.zip(args)) {
      env.define(name.lexeme, arg);
    }

    try
      interpreter.executeBlock(declaration.body, env);
    catch
      case e: Return => {
        if isInitializer then return closure.getAt(0, "this");
        return e.value
      }

    if isInitializer then return closure.getAt(0, "this")
    return null;
  }

  def bind(instance: LoxInstance): LoxFunction = {
    val env = Environment(closure);
    env.define("this", instance);
    LoxFunction(declaration, env, isInitializer)
  }

  override def toString(): String = "<fn " + declaration.name.lexeme + ">";
