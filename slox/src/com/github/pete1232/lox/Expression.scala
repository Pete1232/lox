package com.github.pete1232.lox

import com.github.pete1232.lox.Token.OperatorToken

sealed trait Expression

object Expression:

  final case class Literal(
      value: Token.LiteralNumber | Token.LiteralString | Token.SimpleToken
  ) extends Expression

  final case class Group(
      expression: Expression
  ) extends Expression

  final case class Unary(
      operator: OperatorToken,
      right: Expression,
  ) extends Expression

  final case class Binary(
      left: Expression,
      operator: Token.OperatorToken,
      right: Expression,
  ) extends Expression
