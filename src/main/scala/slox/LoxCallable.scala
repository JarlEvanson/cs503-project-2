package slox;

trait LoxCallable:
  def arity(): Int
  def call(interpreter: Interpreter, args: Array[Any]): Any;
