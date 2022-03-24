package com.github.pete1232.lox

import weaver.SimpleIOSuite
import cats.Show

object ExpressionSuite extends SimpleIOSuite:

  pureTest("show an expression") {
    val expression =
      Expression.Binary(
        operator = Token.SingleCharacter.Star,
        left = Expression.Unary(
          operator = Token.SingleCharacter.Minus,
          right = Expression.Literal(Token.LiteralNumber("123", 123)),
        ),
        right = Expression.Group(
          Expression.Literal(Token.LiteralNumber("45.67", 45.67))
        ),
      )

    val result = Show[Expression].show(expression)

    expect(result == "(* (- 123) (group 45.67))")
  }
