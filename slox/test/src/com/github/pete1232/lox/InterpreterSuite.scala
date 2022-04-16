package com.github.pete1232.lox

import com.github.pete1232.lox.Interpreter.given
import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.utils.Showable.given

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object InterpreterSuite extends SimpleIOSuite with Checkers:

  val loxValueGen: Gen[LoxValue] = Gen.oneOf(
    Gen.double,
    Gen.alphaNumStr,
    Gen.oneOf(true, false),
    Gen.const(null),
  )

  given ExpressionContext = ExpressionContext(0)

  test("evaluating a literal should return its value") {
    forall(loxValueGen) { v =>
      expect(Expression.Literal(v).interpret == Right(v))
    }
  }

  test("evaluating a group should evaluate an inner literal expression") {
    forall(loxValueGen) { v =>
      val expr = Expression.Group(Expression.Literal(v))
      expect(expr.interpret == expr.expression.interpret)
    }
  }
  // todo group with other nested expressions

  test("evaluate a unary `-` expression with a numeric right operand") {
    forall(Gen.double) { v =>
      val expr =
        Expression.Unary(Token.SingleCharacter.Minus, Expression.Literal(v))
      expect(expr.interpret == Right(-v))
    }
  }

  pureTest("error when the right operand of a `-` unary is not a number") {
    val expr = Expression.Unary(
      Token.SingleCharacter.Minus,
      Expression.Literal("teststring"),
    )
    expect(
      expr.interpret == Left(
        InterpreterError.UnaryCastError(
          "teststring",
          Token.SingleCharacter.Minus,
        )
      )
    )
  }
  // todo unary with `!`
  // todo unary with other nested expressions
end InterpreterSuite
