package com.github.pete1232.lox

import com.github.pete1232.lox.Interpreter.given

import cats.Show
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

  given Show[LoxValue] = Show.fromToString

  test("evaluating a literal should return its value")(
    forall(loxValueGen) { v =>
      expect(Expression.Literal(v).interpret == v)
    }
  )
