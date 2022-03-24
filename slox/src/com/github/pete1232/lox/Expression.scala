package com.github.pete1232.lox

import com.github.pete1232.lox.Token

sealed trait Expression

object Expression:

  final case class Literal(
      value: Token
  ) extends Expression

  final case class Group(
      expression: Expression
  ) extends Expression

  final case class Unary(
      operator: Token,
      right: Expression,
  ) extends Expression

  final case class Binary(
      left: Expression,
      operator: Token,
      right: Expression,
  ) extends Expression
