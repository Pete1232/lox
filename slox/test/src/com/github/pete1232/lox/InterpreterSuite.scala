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

  given Arbitrary[LoxValue] = Arbitrary(
    Gen.oneOf(
      Gen.double,
      Gen.alphaNumStr,
      Gen.oneOf(true, false),
      Gen.const(null),
    )
  )

  given ExpressionContext = ExpressionContext(0)

  val genUnaryToken: Gen[
    (Token.SingleCharacter.Bang.type | Token.SingleCharacter.Minus.type)
  ] = Gen.oneOf(
    Token.SingleCharacter.Bang,
    Token.SingleCharacter.Minus,
  )

  // todo implement remaining expresions
  given Arbitrary[Expression] =
    Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[Expression.Literal],
        Arbitrary.arbitrary[Expression.Unary],
      )
    )

  given Arbitrary[Expression.Literal] =
    Arbitrary {
      for value <- Arbitrary.arbitrary[LoxValue]
      yield Expression.Literal(value)
    }

  given Arbitrary[Expression.Unary] =
    Arbitrary {
      Arbitrary.arbitrary[LoxValue].flatMap {
        case d: Double =>
          genUnaryToken
            .map(token => Expression.Unary(token, Expression.Literal(d)))
        case other     =>
          Expression
            .Unary(Token.SingleCharacter.Bang, Expression.Literal(other))
      }
    }

  test("evaluating a literal should return its value") {
    forall { (v: LoxValue) =>
      for result <- Expression.Literal(v).interpret
      yield expect(result == v)
    }
  }

  test("evaluating a group should evaluate an inner expression") {
    forall { (expr: Expression) =>
      for
        groupResult <- Expression.Group(expr).interpret
        innerResult <- expr.interpret
      yield expect(groupResult == innerResult)
    }
  }

  test("evaluate a unary `-` expression with a numeric right operand") {
    forall { (v: Double) =>
      for result <- Expression
          .Unary(Token.SingleCharacter.Minus, Expression.Literal(v))
          .interpret
      yield expect(result == -v)
    }
  }

  test("throw a runtime error evaluating a unary `-` expression on a string") {
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

  test(s"evaluate a binary `/` expression on double operands") {
    forall { (d1: Double) =>
      forall(Gen.double.filterNot(_ == 0)) { (d2: Double) =>
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
  }

  binaryDoubleTest(Token.SingleCharacter.Minus, _ - _)
  binaryDoubleTest(Token.SingleCharacter.Star, _ * _)
  binaryDoubleTest(Token.SingleCharacter.Plus, _ + _)
  binaryDoubleTest(Token.SingleCharacter.Greater, _ > _)
  binaryDoubleTest(Token.TwoCharacter.GreaterEqual, _ >= _)
  binaryDoubleTest(Token.SingleCharacter.Less, _ < _)
  binaryDoubleTest(Token.TwoCharacter.LessEqual, _ <= _)

  binaryDoubleError(Token.SingleCharacter.Slash)
  binaryDoubleError(Token.SingleCharacter.Minus)
  binaryDoubleError(Token.SingleCharacter.Star)
  binaryDoubleError(Token.SingleCharacter.Greater)
  binaryDoubleError(Token.TwoCharacter.GreaterEqual)
  binaryDoubleError(Token.SingleCharacter.Less)
  binaryDoubleError(Token.TwoCharacter.LessEqual)

  test("`/` should throw a runtime error on divide by 0") {
    forall { (d: Double) =>
      val result = Expression
        .Binary(
          Expression.Literal(d),
          Token.SingleCharacter.Slash,
          Expression.Literal(0),
        )
        .interpret
      expectError(result) { case error: InterpreterError.DivideByZero =>
        expect(
          error == InterpreterError.DivideByZero(summon[ExpressionContext].line)
        )
      }
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

  test("`+` should be able to automatically convert operands to strings") {
    forall { (v1: String, v2: LoxValue) =>
      for
        r1 <- Expression
          .Binary(
            Expression.Literal(v1),
            Token.SingleCharacter.Plus,
            Expression.Literal(v2),
          )
          .interpret
        r2 <- Expression
          .Binary(
            Expression.Literal(v2),
            Token.SingleCharacter.Plus,
            Expression.Literal(v1),
          )
          .interpret
      yield expect.all(r1 == v1 + v2.show, r2 == v2.show + v1)
    }
  }

  test("evaluate a binary `==` expression on equal operands") {
    forall { (expr: Expression) =>
      for result <- Expression
          .Binary(
            expr,
            Token.TwoCharacter.EqualEqual,
            expr,
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
    forall { (expr: Expression) =>
      for result <- Expression
          .Binary(
            expr,
            Token.TwoCharacter.BangEqual,
            expr,
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

  test("evaluate a ternary conditional expression") {
    forall { (e1: Expression, e2: Expression) =>
      for
        v1 <- e1.interpret
        v2 <- e2.interpret
        r1 <- Expression
          .Ternary(
            Expression.Literal(true),
            e1,
            e2,
          )
          .interpret
        r2 <- Expression
          .Ternary(
            Expression.Literal(false),
            e1,
            e2,
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

  test("evaluate a comma expression") {
    forall { (e1: Expression, e2: Expression) =>
      for
        v2     <- e2.interpret
        result <- Expression
          .Binary(
            e1,
            Token.SingleCharacter.Comma,
            e2,
          )
          .interpret
      yield expect(result == v2)
    }
  }

  test(
    "throw a runtime error if the left hand operand of a comma expression is invalid"
  ) {
    forall { (expr: Expression) =>
      val result = Expression
        .Binary(
          Expression
            .Unary(Token.SingleCharacter.Minus, Expression.Literal("test")),
          Token.SingleCharacter.Comma,
          expr,
        )
        .interpret
      expectError(result) { case _: InterpreterError.UnaryCastError => success }
    }
  }

  private def expectError[T](
      result: IO[T]
  )(expectation: Throwable => Expectations) =
    result
      .map(_ =>
        failure("Expected a runtime error but the interpreter succeeded")
      )
      .handleError(expectation)
      .handleError { case t: Throwable =>
        failure(
          s"Throwable did not match the expectation. Found ${t.getMessage}"
        )
      }

  private def binaryDoubleTest[T <: LoxValue](
      token: Expression.BinaryOperator,
      op: (Double, Double) => T,
  ) =
    test(s"evaluate a binary `${token.lexeme}` expression on double operands") {
      forall { (d1: Double, d2: Double) =>
        for result <- Expression
            .Binary(
              Expression.Literal(d1),
              token,
              Expression.Literal(d2),
            )
            .interpret
        yield expect(result == op(d1, d2))
      }
    }

  private def binaryDoubleError(token: Expression.BinaryOperator) =
    test(
      s"throw a runtime error evaluating a binary `${token.lexeme}` expression where at least one operand isn't a double"
    ) {
      forall { (expr: Expression, s: String) =>
        val result =
          Expression
            .Binary(expr, token, Expression.Literal(s))
            .interpret
        expectError(result) { case error: InterpreterError.BinaryCastError =>
          success
        }
      }
    }

end InterpreterSuite
