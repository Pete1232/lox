package com.github.pete1232.lox

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import org.scalacheck.Gen
import com.github.pete1232.lox.errors.ParserError

object ParserSuite extends SimpleIOSuite with Checkers:

  test("primary rule can match a literal string") {
    forall(Gen.alphaNumStr) { stringLiteral =>
      val tokens =
        List(Token.LiteralString(s"\"${stringLiteral}\"", stringLiteral))
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(stringLiteral))))
    }
  }

  test("primary rule can match a literal number") {
    forall(Gen.double) { numberLiteral =>
      val tokens =
        List(Token.LiteralNumber(numberLiteral.toString, numberLiteral))
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(numberLiteral))))
    }
  }

  pureTest("primary rule can match a boolean") {
    val tokens =
      List(
        Token.Keyword.True,
        Token.Keyword.False,
      )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(Expression.Literal(true)),
        Right(Expression.Literal(false)),
      )
    )
  }

  pureTest("primary rule can match a nil value") {
    val tokens =
      List(
        Token.Keyword.Nil
      )
    val result = DefaultParser.parse(tokens)
    expect(result == List(Right(Expression.Literal(null))))
  }

  pureTest("primary rule should error for a token that is not handled") {
    val token  = Token.Keyword.And
    val tokens =
      List(token)
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(Left(ParserError.UnmatchedTokenError("primary", 0, token)))
    )
  }

end ParserSuite
