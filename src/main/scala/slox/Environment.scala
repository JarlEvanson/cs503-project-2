package slox;

import scala.collection.mutable.Map

class Environment(enclosing: Environment):
  def this() = this(null);

  var values = Map[String, Any]();

  def define(name: String, value: Any): Unit = values += (name, value);
  def get(name: Token): Any = {
    if values.contains(name.lexeme) then return values(name.lexeme);
    if enclosing != null then return enclosing.get(name);
    throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
  }

  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)) {
      values(name.lexeme) = value;
      return;
    }

    if enclosing != null then return enclosing.assign(name, value);

    throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
  }
