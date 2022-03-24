package com.github.pete1232.lox

import com.github.pete1232.lox.Token

sealed trait Expression

object Expression:

  final case class Literal(
      value: Token.LiteralNumber | Token.LiteralString |
        Token.Keyword.True.type | Token.Keyword.False.type |
        Token.Keyword.Nil.type
  ) extends Expression

  final case class Group(
      expression: Expression
  ) extends Expression

  final case class Unary(
      operator: Token.SingleCharacter.Minus.type |
        Token.SingleCharacter.Bang.type,
      right: Expression,
  ) extends Expression

  final case class Binary(
      left: Expression,
      operator: Token.TwoCharacter | Token.SingleCharacter.Less.type |
        Token.SingleCharacter.Greater.type | Token.SingleCharacter.Plus.type |
        Token.SingleCharacter.Star.type | Token.SingleCharacter.Slash.type,
      right: Expression,
  ) extends Expression
