package com.github.pete1232.lox

import com.github.pete1232.lox.Token
import com.github.pete1232.lox.utils.Showable

sealed trait Expression:
  def context: ExpressionContext

object Expression:

  given Showable[Expression] with
    extension (e: Expression)
      def show: String =
        e match
          case l: Literal => l.value.show
          case g: Group   => "(" + "group " + g.expression.show + ")"
          case u: Unary   => "(" + u.operator.lexeme + " " + u.right.show + ")"
          case b: Binary  =>
            "(" + b.operator.lexeme + " " + b.left.show + " " + b.right.show + ")"
          case t: Ternary =>
            "(" + t.left.show + " ? " + t.middle.show + " : " + t.right.show + ")"

  final case class Literal(value: LoxValue)(using
      val context: ExpressionContext
  ) extends Expression

  final case class Group(
      expression: Expression
  )(using val context: ExpressionContext)
      extends Expression

  final case class Unary(
      operator: Token.SingleCharacter.Minus.type |
        Token.SingleCharacter.Bang.type,
      right: Expression,
  )(using val context: ExpressionContext)
      extends Expression

  type BinaryOperator = Token.TwoCharacter | Token.SingleCharacter.Less.type |
    Token.SingleCharacter.Greater.type | Token.SingleCharacter.Plus.type |
    Token.SingleCharacter.Minus.type | Token.SingleCharacter.Star.type |
    Token.SingleCharacter.Slash.type | Token.SingleCharacter.Comma.type

  final case class Binary(
      left: Expression,
      operator: BinaryOperator,
      right: Expression,
  )(using val context: ExpressionContext)
      extends Expression

  final case class Ternary(
      left: Expression,
      middle: Expression,
      right: Expression,
  )(using val context: ExpressionContext)
      extends Expression

final case class ExpressionContext(line: Int)

object ExpressionContext:
  def apply(tokenContext: TokenContext): ExpressionContext = apply(
    tokenContext.lineCount
  )
