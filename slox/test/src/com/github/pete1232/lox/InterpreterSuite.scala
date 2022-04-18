package com.github.pete1232.lox

import com.github.pete1232.lox.Interpreter.given
import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.models.{Expression, ExpressionContext, Token}
import com.github.pete1232.lox.utils.Showable.given

import cats.effect.IO
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import weaver.Expectations
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
      for result <- Expression.Literal(v).interpret
      yield expect(result == Right(v))
    }
  }

  test("evaluating a group should evaluate an inner literal expression") {
    forall(loxValueGen) { v =>
      for
        groupResult   <- Expression.Group(Expression.Literal(v)).interpret
        literalResult <- Expression.Literal(v).interpret
      yield expect(groupResult == literalResult)
    }
  }
  // todo group with other nested expressions

  test("evaluate a unary `-` expression with a numeric right operand") {
    forall(Gen.double) { v =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Minus, Expression.Literal(v))
          .interpret
      yield expect(result == Right(-v))
    }
  }

  test("throw a runtime error evaluating a unary expression on a string") {
    val result = Expression
      .Unary(
        Token.SingleCharacter.Minus,
        Expression.Literal("teststring"),
      )
      .interpret
    expectError(result) { case err: InterpreterError.UnaryCastError =>
      expect(
        err == InterpreterError.UnaryCastError(
          "teststring",
          Token.SingleCharacter.Minus,
        )
      )
    }
  }

  test("evaluate a unary `!` expression with a boolean right operand") {
    forall(Gen.oneOf(true, false)) { v =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Bang, Expression.Literal(v))
          .interpret
      yield expect(result == Right(!v))
    }
  }

  test(
    "evaluate a unary `!` expression with a null right operand, treating null as false"
  ) {
    for result <- Expression
        .Unary(Token.SingleCharacter.Bang, Expression.Literal(null))
        .interpret
    yield expect(result == Right(true))
  }

  test(
    "evaluate a unary `!` expression with a string right operand, treating any string as true"
  ) {
    forall(Gen.alphaNumStr) { s =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Bang, Expression.Literal(s))
          .interpret
      yield expect(result == Right(false))
    }
  }
  // todo unary with non-literal expressions

  test("evaluate a binary `-` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Minus,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == Right(d1 - d2))
    }
  }

  // test("evaluate a binary `/` expression on double operands") {
  //   forall { (d1: Double, d2: Double) =>
  //     for result <- Expression
  //         .Binary(
  //           Expression.Literal(d1),
  //           Token.SingleCharacter.Slash,
  //           Expression.Literal(d2),
  //         )
  //         .interpret
  //     yield expect(result == Right(d1 / d2))
  //   }
  // }

  // test("evaluate a binary `*` expression on double operands") {
  //   forall { (d1: Double, d2: Double) =>
  //     for result <- Expression
  //         .Binary(
  //           Expression.Literal(d1),
  //           Token.SingleCharacter.Star,
  //           Expression.Literal(d2),
  //         )
  //         .interpret
  //     yield expect(result == Right(d1 * d2))
  //   }
  // }

  test("throw a runtime error evaluating a binary expression on a string") {
    val result = Expression
      .Binary(
        Expression.Literal("hello"),
        Token.SingleCharacter.Minus,
        Expression.Literal("world"),
      )
      .interpret
    expectError(result) { case error: InterpreterError.BinaryCastError =>
      expect(
        error == InterpreterError.BinaryCastError(
          "hello",
          "world",
          Token.SingleCharacter.Minus,
        )
      )
    }
  }
  // todo binary with non-literal expressions

  private def expectError[T](
      result: IO[T]
  )(expectation: Throwable => Expectations) =
    result
      .map(_ => failure("expected a runtime error"))
      .handleError(expectation)

end InterpreterSuite
