package slox;

import scala.collection.mutable.Map

class LoxInstance(klass: LoxClass):
  val fields = Map[String, Any]();

  def get(name: Token): Any = {
    if fields.contains(name.lexeme) then return fields(name.lexeme);
    
    val method = klass.findMethod(name.lexeme);
    if method != null then return method.bind(this);

    throw RuntimeError(name, "Undefined property '" + name.lexeme + "'.");
  }
  def set(name: Token, value: Any): Unit = fields(name.lexeme) = value;

  override def toString(): String = klass.name + " instance";
