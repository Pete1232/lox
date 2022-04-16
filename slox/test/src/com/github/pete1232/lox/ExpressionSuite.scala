package com.github.pete1232.lox

import weaver.SimpleIOSuite

object ExpressionSuite extends SimpleIOSuite:

  given ExpressionContext = ExpressionContext(0)

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

    val result = expression.show

    expect(result == "(* (- 123.0) (group 45.67))")
  }
