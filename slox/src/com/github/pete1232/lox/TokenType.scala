package com.github.pete1232.lox

sealed trait TokenType

object TokenType:

  enum SingleCharacter extends TokenType:
    case LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus, Plus,
      Semicolon, Slash, Star, Bang, Equal, Greater, Less

  enum TwoCharacter extends TokenType:
    case BangEqual, EqualEqual, GreaterEqual, LessEqual

  enum Literal extends TokenType:
    case Identifier, String, Number

  enum Keywords extends TokenType:
    case And, Class, Else, False, Fun, For, If, Nil, Or, Print, Return, Super,
      This, True, Var, While, EOF
