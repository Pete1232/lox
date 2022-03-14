package com.github.pete1232.lox

import scala.collection.View.Single
import cats.Show

sealed trait TokenType

class FixedTokenType(val lexeme: String) extends TokenType:
  final val length = lexeme.length

object TokenType:

  enum SingleCharacter(lexeme: String) extends FixedTokenType(lexeme):
    case LeftParen  extends SingleCharacter("(")
    case RightParen extends SingleCharacter(")")
    case LeftBrace  extends SingleCharacter("{")
    case RightBrace extends SingleCharacter("}")
    case Comma      extends SingleCharacter(",")
    case Dot        extends SingleCharacter(".")
    case Minus      extends SingleCharacter("-")
    case Plus       extends SingleCharacter("+")
    case Semicolon  extends SingleCharacter(";")
    case Slash      extends SingleCharacter("/")
    case Star       extends SingleCharacter("*")
    case Bang       extends SingleCharacter("!")
    case Equal      extends SingleCharacter("=")
    case Greater    extends SingleCharacter(">")
    case Less       extends SingleCharacter("<")

  object SingleCharacter:
    implicit val showSingleCharacter: Show[TokenType.SingleCharacter] =
      Show.fromToString

    def fromString(s: String): Option[TokenType.SingleCharacter] =
      SingleCharacter.values.find(_.lexeme == s)

  enum TwoCharacter(lexeme: String) extends FixedTokenType(lexeme):
    case BangEqual    extends TwoCharacter("!=")
    case EqualEqual   extends TwoCharacter("==")
    case GreaterEqual extends TwoCharacter(">=")
    case LessEqual    extends TwoCharacter("<=")

  object TwoCharacter:
    implicit val showTwoCharacter: Show[TokenType.TwoCharacter] =
      Show.fromToString

    final val entrypoints = List('!', '=', '>', '<')

    def fromString(s: String): Option[TokenType.TwoCharacter] =
      TwoCharacter.values.find(_.lexeme == s)

  enum Literal extends TokenType:
    case Identifier, StringLiteral, NumberLiteral

  enum Keyword(lexeme: String) extends FixedTokenType(lexeme):
    case And    extends Keyword("and")
    case Class  extends Keyword("class")
    case Else   extends Keyword("else")
    case False  extends Keyword("false")
    case Fun    extends Keyword("fun")
    case For    extends Keyword("for")
    case If     extends Keyword("if")
    case Nil    extends Keyword("nil")
    case Or     extends Keyword("or")
    case Print  extends Keyword("print")
    case Return extends Keyword("return")
    case Super  extends Keyword("super")
    case This   extends Keyword("this")
    case True   extends Keyword("true")
    case Var    extends Keyword("var")
    case While  extends Keyword("while")
    case EOF    extends Keyword("eof")

  object Keyword:
    def fromString(s: String): Option[TokenType.Keyword] =
      Keyword.values.find(_.lexeme == s)
end TokenType
