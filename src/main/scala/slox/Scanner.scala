package slox;

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class Scanner(source: String):
  var tokens = ArrayBuffer[Token]();
  var start = 0;
  var current = 0;
  var line = 1;

  val keywords = {
    var map = HashMap[String, TokenType]();

    map("and") = TokenType.And;
    map("class") = TokenType.Class;
    map("else") = TokenType.Else;
    map("false") = TokenType.False;
    map("for") = TokenType.For;
    map("fun") = TokenType.Fun;
    map("if") = TokenType.If;
    map("nil") = TokenType.Nil;
    map("or") = TokenType.Or;
    map("print") = TokenType.Print;
    map("return") = TokenType.Return;
    map("super") = TokenType.Super;
    map("this") = TokenType.This;
    map("true") = TokenType.True;
    map("var") = TokenType.Var;
    map("while") = TokenType.While;

    map
  };

  def scanTokens(): Array[Token] = {
    while (!isAtEnd()) {
      // We are at the beginning of the next lexeme.
      start = current;
      scanToken();
    }

    tokens += Token(TokenType.Eof, "", null, line)
    tokens.toArray
  }

  def scanToken(): Unit = {
    val c = advance();
    c match
      case '(' => addToken(TokenType.LeftParen);
      case ')' => addToken(TokenType.RightParen);
      case '{' => addToken(TokenType.LeftBrace);
      case '}' => addToken(TokenType.RightBrace);
      case ',' => addToken(TokenType.Comma);
      case '.' => addToken(TokenType.Dot);
      case '-' => addToken(TokenType.Minus);
      case '+' => addToken(TokenType.Plus);
      case ';' => addToken(TokenType.Semicolon);
      case '*' => addToken(TokenType.Star);
      case '!' => addToken(if matches('=') then TokenType.BangEqual else TokenType.Bang);
      case '=' => addToken(if matches('=') then TokenType.EqualEqual else TokenType.Equal);
      case '<' => addToken(if matches('=') then TokenType.LessEqual else TokenType.Less);
      case '>' => addToken(if matches('=') then TokenType.GreaterEqual else TokenType.Greater);
      case '/' => {
        if (matches('/'))
          // Skip all characters until end of line (comment).
          while (peek() != '\n' && !isAtEnd()) { advance(); }
        else
          addToken(TokenType.Slash);
      }
      case ' ' | '\r' | '\t' => {} // Skip whitespace
      case '\n' => line += 1;
      case '"' => parseString();

      case _   => {
        if isDigit(c) then
          parseNumber();
        else if isAlpha(c) then
          parseIdentifier();
        else
          Lox.error(line, "Unexpected character.")
      }
  }

  def parseIdentifier(): Unit = {
    while (isAlphaNumeric(peek())) advance();

    val text = source.substring(start, current);
    val kind = keywords.get(text) match
      case Some(a) => a
      case None => TokenType.Identifier

    addToken(kind)
  }

  def parseNumber(): Unit = {
    while (isDigit(peek())) advance();

    if (peek() == '.' && isDigit(peekNext())) {
      advance(); // Consume the ".".

      while (isDigit(peek())) advance();
    }

    addToken(TokenType.Number, source.substring(start, current).toDouble)
  }

  def parseString(): Unit = {
    while (peek() != '"' && !isAtEnd()) {
      if peek() == '\n' then line += 1;
      advance();
    }

    if (isAtEnd())
      Lox.error(line, "Unterminated string.");
      return;

    // Capture the closing ".
    advance();

    val value = source.substring(start + 1, current - 1);
    addToken(TokenType.String, value);
  }

  def matches(expected: Char): Boolean = {
    if (isAtEnd()) return false;
    if (source.charAt(current) != expected) return false;

    current += 1;
    true
  }

  def peek(): Char = {
    if (isAtEnd()) return '\u0000';
    source.charAt(current)
  }

  def peekNext(): Char = {
    if (current + 1 >= source.length) return '\u0000';
    source.charAt(current + 1)
  }

  def isAlphaNumeric(c: Char): Boolean = {
    isAlpha(c) || isDigit(c)
  }

  def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || c == '_'
  }

  def isDigit(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  def isAtEnd(): Boolean = {
    current >= source.length()
  }

  def advance(): Char = {
    val c = source.charAt(current);
    current += 1;
    c
  }

  def addToken(kind: TokenType): Unit = {
    addToken(kind, null);
  }

  def addToken(kind: TokenType, literal: Any): Unit = {
    val text = source.substring(start, current);
    tokens += Token(kind, text, literal, line)
  }
