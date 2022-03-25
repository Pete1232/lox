package com.github.pete1232.lox

import cats.Show
import weaver.SimpleIOSuite

object ExpressionSuite extends SimpleIOSuite:

  pureTest("show an expression") {
    val expression =
      Expression.Binary(
        operator = Token.SingleCharacter.Star,
        left = Expression.Unary(
          operator = Token.SingleCharacter.Minus,
          right = Expression.Literal(123),
        ),
        right = Expression.Group(
          Expression.Literal(45.67)
        ),
      )

    val result = Show[Expression].show(expression)

    expect(result == "(* (- 123.0) (group 45.67))")
  }
