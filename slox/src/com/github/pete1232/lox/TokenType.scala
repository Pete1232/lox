package com.github.pete1232.lox

import scala.collection.View.Single

sealed trait TokenType

object TokenType:

  enum SingleCharacter(val lexeme: String) extends TokenType:
    case LeftParen extends SingleCharacter("(")
    case RightParen extends SingleCharacter(")")
    case LeftBrace extends SingleCharacter("{")
    case RightBrace extends SingleCharacter("}")
    case Comma extends SingleCharacter(",")
    case Dot extends SingleCharacter(".")
    case Minus extends SingleCharacter("-")
    case Plus extends SingleCharacter("+")
    case Semicolon extends SingleCharacter(";")
    case Slash extends SingleCharacter("/")
    case Star extends SingleCharacter("*")
    case Bang extends SingleCharacter("!")
    case Equal extends SingleCharacter("=")
    case Greater extends SingleCharacter(">")
    case Less extends SingleCharacter("<")

  object SingleCharacter:
    def fromString(s: String): Option[TokenType] =
      SingleCharacter.values.find(_.lexeme == s)

  enum TwoCharacter(val lexeme: String) extends TokenType:
    case BangEqual extends TwoCharacter("!=")
    case EqualEqual extends TwoCharacter("==")
    case GreaterEqual extends TwoCharacter(">=")
    case LessEqual extends TwoCharacter("<=")

  object TwoCharacter:
    def fromString(s: String): Option[TokenType] =
      TwoCharacter.values.find(_.lexeme == s)

  enum Literal:
    case Identifier, StringLiteral, NumberLiteral

  enum Keyword(val lexeme: String) extends TokenType:
    case And extends Keyword("and")
    case Class extends Keyword("class")
    case Else extends Keyword("else")
    case False extends Keyword("false")
    case Fun extends Keyword("fun")
    case For extends Keyword("for")
    case If extends Keyword("if")
    case Nil extends Keyword("nil")
    case Or extends Keyword("or")
    case Print extends Keyword("print")
    case Return extends Keyword("return")
    case Super extends Keyword("super")
    case This extends Keyword("this")
    case True extends Keyword("true")
    case Var extends Keyword("var")
    case While extends Keyword("while")
    case EOF extends Keyword("eof")

  object Keyword:
    def fromString(s: String): Option[TokenType] =
      Keyword.values.find(_.lexeme == s)
