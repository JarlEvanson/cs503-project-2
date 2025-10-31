package slox;

import slox.TokenType

class Token(val kind: TokenType, val lexeme: String, val literal: Any, val line: Int):
  override def toString(): String = {
    s"$kind $lexeme $literal"
  }
