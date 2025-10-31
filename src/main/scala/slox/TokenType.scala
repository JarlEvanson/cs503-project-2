package slox;

enum TokenType:
  // Single-character tokens.
  case LeftParen, RightParen, LeftBrace, RightBrace
  case Comma, Dot, Minus, Plus, Semicolon, Slash, Star

  // One or two character tokens.
  case Bang, BangEqual
  case Equal, EqualEqual
  case Greater, GreaterEqual
  case Less, LessEqual

  // Literals
  case Identifier, String, Number

  // Keywords
  case And, Class, Else, False, Fun, For, If, Nil, Or
  case Print, Return, Super, This, True, Var, While

  case Eof
