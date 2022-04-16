package com.github.pete1232.lox

import com.github.pete1232.lox.utils.Showable

sealed trait Token:
  def lexeme: String
  final val length: Int = lexeme.length

object Token:

  given Showable[Token] with
    extension (t: Token)
      def show: String =
        val className = t.getClass.getSimpleName
        val out       = if className != null then className else "Unknown"
        s"$out for lexeme ${t.lexeme}"

  enum SingleCharacter(final val lexeme: String) extends Token:
    case LeftParen  extends SingleCharacter("(")
    case RightParen extends SingleCharacter(")")
    case LeftBrace  extends SingleCharacter("{")
    case RightBrace extends SingleCharacter("}")
    case Comma      extends SingleCharacter(",")
    case Dot        extends SingleCharacter(".")
    case Semicolon  extends SingleCharacter(";")
    case Minus      extends SingleCharacter("-")
    case Plus       extends SingleCharacter("+")
    case Slash      extends SingleCharacter("/")
    case Star       extends SingleCharacter("*")
    case Equal      extends SingleCharacter("=")
    case Greater    extends SingleCharacter(">")
    case Less       extends SingleCharacter("<")
    case Bang       extends SingleCharacter("!")
    case Question   extends SingleCharacter("?")
    case Colon      extends SingleCharacter(":")

  object SingleCharacter:
    def fromString(s: String): Option[SingleCharacter] =
      SingleCharacter.values.find(_.lexeme == s)

  enum TwoCharacter(final val lexeme: String) extends Token:
    case BangEqual    extends TwoCharacter("!=")
    case EqualEqual   extends TwoCharacter("==")
    case GreaterEqual extends TwoCharacter(">=")
    case LessEqual    extends TwoCharacter("<=")

  object TwoCharacter:
    final val entrypoints = List('!', '=', '>', '<')

    def fromString(s: String): Option[TwoCharacter] =
      TwoCharacter.values.find(_.lexeme == s)

  final case class LiteralString(
      lexeme: String,
      literal: String,
  ) extends Token

  final case class LiteralNumber(
      lexeme: String,
      literal: Double,
  ) extends Token

  final case class LiteralIdentifier(
      lexeme: String,
      literal: String,
  ) extends Token

  enum Keyword(final val lexeme: String) extends Token:
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
    def fromString(s: String): Option[Keyword] =
      Keyword.values.find(_.lexeme == s)

end Token

final case class TokenWithContext(token: Token, context: TokenContext)

object TokenWithContext:
  given Showable[TokenWithContext] with
    extension (tc: TokenWithContext) def show: String = tc.token.show

final case class TokenContext(lineCount: Int)
