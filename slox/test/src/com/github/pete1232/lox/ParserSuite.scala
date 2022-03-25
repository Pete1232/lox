package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ParserSuite extends SimpleIOSuite with Checkers:

  test("primary rule can match a literal string") {
    forall(Gen.alphaNumStr) { stringLiteral =>
      val tokens =
        List(
          TokenWithContext(
            Token.LiteralString(s"\"${stringLiteral}\"", stringLiteral),
            TokenContext(0),
          )
        )
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(stringLiteral))))
    }
  }

  test("primary rule can match a literal number") {
    forall(Gen.double) { numberLiteral =>
      val tokens =
        List(
          TokenWithContext(
            Token.LiteralNumber(numberLiteral.toString, numberLiteral),
            TokenContext(0),
          )
        )
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(numberLiteral))))
    }
  }

  pureTest("primary rule can match a boolean") {
    val tokens =
      List(
        TokenWithContext(Token.Keyword.True, TokenContext(0)),
        TokenWithContext(Token.Keyword.False, TokenContext(0)),
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
        TokenWithContext(Token.Keyword.Nil, TokenContext(0))
      )
    val result = DefaultParser.parse(tokens)
    expect(result == List(Right(Expression.Literal(null))))
  }

  test("primary rule should error for a token that is not handled") {
    forall(Gen.posNum[Int]) { line =>
      val token  = Token.Keyword.And
      val tokens =
        List(TokenWithContext(token, TokenContext(line)))
      val result = DefaultParser.parse(tokens)
      expect(
        result == List(
          Left(ParserError.UnmatchedTokenError("primary", line, token))
        )
      )
    }
  }

end ParserSuite
