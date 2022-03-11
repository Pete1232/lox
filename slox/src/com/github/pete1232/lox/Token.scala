package com.github.pete1232.lox

import cats.Show

final case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Object,
    line: Int
)

object Token:
  implicit val showToken: Show[Token] =
    Show.show(t => s"${t.tokenType} ${t.lexeme} ${t.literal}")
