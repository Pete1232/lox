package com.github.pete1232.lox

import com.github.pete1232.lox.Token
import cats.Show

sealed trait Expression

object Expression:

  implicit val showExpression: Show[Expression] = Show.show(
    _ match
      case l: Literal => Show[Literal].show(l)
      case g: Group   => Show[Group].show(g)
      case u: Unary   => Show[Unary].show(u)
      case b: Binary  => Show[Binary].show(b)
  )

  final case class Literal(
      value: Double | String | Boolean
  ) extends Expression

  object Literal:
    implicit val showLiteral: Show[Literal] = Show.show(_.value.toString)

  final case class Group(
      expression: Expression
  ) extends Expression

  object Group:
    implicit val showGroup: Show[Group] = Show.show { g =>
      "(" + "group " + Show[Expression].show(g.expression) + ")"
    }

  final case class Unary(
      operator: Token.SingleCharacter.Minus.type |
        Token.SingleCharacter.Bang.type,
      right: Expression,
  ) extends Expression

  object Unary:
    implicit val showUnary: Show[Unary] = Show.show { u =>
      "(" + u.operator.lexeme + " " + Show[Expression].show(u.right) + ")"
    }

  final case class Binary(
      left: Expression,
      operator: Token.TwoCharacter | Token.SingleCharacter.Less.type |
        Token.SingleCharacter.Greater.type | Token.SingleCharacter.Plus.type |
        Token.SingleCharacter.Star.type | Token.SingleCharacter.Slash.type,
      right: Expression,
  ) extends Expression

  object Binary:
    implicit val showBinary: Show[Binary] = Show.show { b =>
      "(" + b.operator.lexeme + " " + Show[Expression].show(
        b.left
      ) + " " + Show[Expression].show(b.right) + ")"
    }
end Expression
