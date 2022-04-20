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

  given Arbitrary[LoxValue] = Arbitrary(loxValueGen)

  given ExpressionContext = ExpressionContext(0)

  test("evaluating a literal should return its value") {
    forall { (v: LoxValue) =>
      for result <- Expression.Literal(v).interpret
      yield expect(result == v)
    }
  }

  test("evaluating a group should evaluate an inner literal expression") {
    forall { (v: LoxValue) =>
      for
        groupResult   <- Expression.Group(Expression.Literal(v)).interpret
        literalResult <- Expression.Literal(v).interpret
      yield expect(groupResult == literalResult)
    }
  }
  // todo group with other nested expressions

  test("evaluate a unary `-` expression with a numeric right operand") {
    forall { (v: Double) =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Minus, Expression.Literal(v))
          .interpret
      yield expect(result == -v)
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
          0,
        )
      )
    }
  }

  test("evaluate a unary `!` expression with a boolean right operand") {
    forall { (v: Boolean) =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Bang, Expression.Literal(v))
          .interpret
      yield expect(result == !v)
    }
  }

  test(
    "evaluate a unary `!` expression with a null right operand, treating null as false"
  ) {
    for result <- Expression
        .Unary(Token.SingleCharacter.Bang, Expression.Literal(null))
        .interpret
    yield expect(result == true)
  }

  test(
    "evaluate a unary `!` expression with a string right operand, treating any string as true"
  ) {
    forall { (s: String) =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Bang, Expression.Literal(s))
          .interpret
      yield expect(result == false)
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
      yield expect(result == d1 - d2)
    }
  }

  test("evaluate a binary `/` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Slash,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 / d2)
    }
  }

  test("evaluate a binary `*` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Star,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 * d2)
    }
  }

  test("evaluate a binary `+` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Plus,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 + d2)
    }
  }

  test("evaluate a binary `+` expression on string operands") {
    forall { (s1: String, s2: String) =>
      for result <- Expression
          .Binary(
            Expression.Literal(s1),
            Token.SingleCharacter.Plus,
            Expression.Literal(s2),
          )
          .interpret
      yield expect(result == s1 + s2)
    }
  }

  test("evaluate a binary `>` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Greater,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 > d2)
    }
  }

  test("evaluate a binary `>=` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.TwoCharacter.GreaterEqual,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 >= d2)
    }
  }

  test("evaluate a binary `<` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.SingleCharacter.Less,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 < d2)
    }
  }

  test("evaluate a binary `<=` expression on double operands") {
    forall { (d1: Double, d2: Double) =>
      for result <- Expression
          .Binary(
            Expression.Literal(d1),
            Token.TwoCharacter.LessEqual,
            Expression.Literal(d2),
          )
          .interpret
      yield expect(result == d1 <= d2)
    }
  }

  test("evaluate a binary `==` expression on equal operands") {
    forall { (v: LoxValue) =>
      for result <- Expression
          .Binary(
            Expression.Literal(v),
            Token.TwoCharacter.EqualEqual,
            Expression.Literal(v),
          )
          .interpret
      yield expect(result == true)
    }
  }

  test("evaluate a binary `==` expression on different operands") {
    for result <- Expression
        .Binary(
          Expression.Literal("test"),
          Token.TwoCharacter.EqualEqual,
          Expression.Literal(5),
        )
        .interpret
    yield expect(result == false)
  }

  test("evaluate a binary `==` expression on null operands") {
    for
      r1 <- Expression
        .Binary(
          Expression.Literal(null),
          Token.TwoCharacter.EqualEqual,
          Expression.Literal(null),
        )
        .interpret
      r2 <- Expression
        .Binary(
          Expression.Literal(null),
          Token.TwoCharacter.EqualEqual,
          Expression.Literal("test"),
        )
        .interpret
    yield expect.all(r1 == true, r2 == false)
  }

  test("evaluate a binary `!=` expression on equal operands") {
    forall { (v: LoxValue) =>
      for result <- Expression
          .Binary(
            Expression.Literal(v),
            Token.TwoCharacter.BangEqual,
            Expression.Literal(v),
          )
          .interpret
      yield expect(result == false)
    }
  }

  test("evaluate a binary `!=` expression on different operands") {
    for result <- Expression
        .Binary(
          Expression.Literal("test"),
          Token.TwoCharacter.BangEqual,
          Expression.Literal(5),
        )
        .interpret
    yield expect(result == true)
  }

  test("evaluate a binary `!=` expression on null operands") {
    for
      r1 <- Expression
        .Binary(
          Expression.Literal(null),
          Token.TwoCharacter.BangEqual,
          Expression.Literal(null),
        )
        .interpret
      r2 <- Expression
        .Binary(
          Expression.Literal(null),
          Token.TwoCharacter.BangEqual,
          Expression.Literal("test"),
        )
        .interpret
    yield expect.all(r1 == false, r2 == true)
  }

  test("throw a runtime error evaluating a - binary expression on a string") {
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
          0,
        )
      )
    }
  }
  // todo binary with non-literal expressions
  // todo more comprehensive binary errors

  test("evaluate a ternary conditional expression") {
    forall { (v1: LoxValue, v2: LoxValue) =>
      for
        r1 <- Expression
          .Ternary(
            Expression.Literal(true),
            Expression.Literal(v1),
            Expression.Literal(v2),
          )
          .interpret
        r2 <- Expression
          .Ternary(
            Expression.Literal(false),
            Expression.Literal(v1),
            Expression.Literal(v2),
          )
          .interpret
      yield expect.all(r1 == v1, r2 == v2)
    }
  }

  test(
    "throw a runtime error if the first operand does not evaluate to a boolean"
  ) {
    val result = Expression
      .Ternary(
        Expression.Literal("not a boolean"),
        Expression.Literal(5),
        Expression.Literal(false),
      )
      .interpret
    expectError(result) { case error: InterpreterError.TernaryCastError =>
      expect(
        error == InterpreterError.TernaryCastError(
          "not a boolean",
          summon[ExpressionContext].line,
        )
      )
    }
  }
  // todo ternary with non-literal expressions

  private def expectError[T](
      result: IO[T]
  )(expectation: Throwable => Expectations) =
    result
      .map(_ => failure("expected a runtime error"))
      .handleError(expectation)

end InterpreterSuite
