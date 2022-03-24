package com.github.pete1232.lox

import cats.Show

sealed trait TokenType

trait FixedTokenType extends TokenType:
  def lexeme: String
  final val length = lexeme.length

trait OperatorType extends FixedTokenType

object OperatorType:
  implicit val showOperatorType: Show[OperatorType] = Show.fromToString

object TokenType:

  enum SingleCharacter(final val lexeme: String) extends FixedTokenType:
    case LeftParen  extends SingleCharacter("(")
    case RightParen extends SingleCharacter(")")
    case LeftBrace  extends SingleCharacter("{")
    case RightBrace extends SingleCharacter("}")
    case Comma      extends SingleCharacter(",")
    case Dot        extends SingleCharacter(".")
    case Semicolon  extends SingleCharacter(";")
    case Minus      extends SingleCharacter("-") with OperatorType
    case Plus       extends SingleCharacter("+") with OperatorType
    case Slash      extends SingleCharacter("/") with OperatorType
    case Star       extends SingleCharacter("*") with OperatorType
    case Equal      extends SingleCharacter("=") with OperatorType
    case Greater    extends SingleCharacter(">") with OperatorType
    case Less       extends SingleCharacter("<") with OperatorType

  object SingleCharacter:
    implicit val showSingleCharacter: Show[TokenType.SingleCharacter] =
      Show.fromToString

    def fromString(s: String): Option[TokenType.SingleCharacter] =
      SingleCharacter.values.find(_.lexeme == s)

  enum TwoCharacter(final val lexeme: String) extends OperatorType:
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

  enum Keyword(val lexeme: String) extends FixedTokenType:
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
