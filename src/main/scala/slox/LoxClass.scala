package slox;

import scala.collection.mutable.Map

class LoxClass(val name: String, val methods: Map[String, LoxFunction]) extends LoxCallable:
  def findMethod(name: String): LoxFunction = {
    if methods.contains(name) then
      return methods(name);

    return null;
  }

  def arity(): Int = {
    val initializer = findMethod("init");
    if initializer == null then return 0
    initializer.arity()
  }
  def call(interpreter: Interpreter, args: Array[Any]): Any = {
    var instance = LoxInstance(this);

    val initializer = findMethod("init");
    if initializer != null then
      initializer.bind(instance).call(interpreter, args);

    instance
  }

  override def toString(): String = name;
