package slox;

import scala.collection.mutable.Map

class Environment(private var enclosing: Environment):
  def this() = this(null);

  var values = Map[String, Any]();

  def define(name: String, value: Any): Unit = values += (name, value);
  def get(name: Token): Any = {
    if values.contains(name.lexeme) then return values(name.lexeme);
    if enclosing != null then return enclosing.get(name);
    throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
  }
  def getAt(distance: Int, name: String): Any = ancestor(distance).values(name);
  def ancestor(distance: Int): Environment = {
    var env = this;

    var i = 0;
    while (i < distance) {
      env = env.enclosing;
      i += 1;
    }

    env
  }

  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)) {
      values(name.lexeme) = value;
      return;
    }

    if enclosing != null then return enclosing.assign(name, value);

    throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
  }
  def assignAt(distance: Int, name: Token, value: Any): Unit =
    ancestor(distance).values.put(name.lexeme, value);
