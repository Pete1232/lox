package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

import cats.implicits.*
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ParserSuite extends SimpleIOSuite with Checkers:

  private def simpleToken(token: Token): TokenWithContext =
    TokenWithContext(token, TokenContext(0))

  private def simpleTokens(tokens: Token*): List[TokenWithContext] =
    tokens.map(simpleToken).toList

  test("match a literal string") {
    forall(Gen.alphaNumStr) { stringLiteral =>
      val tokens =
        simpleTokens(
          Token.LiteralString(s"\"${stringLiteral}\"", stringLiteral)
        )
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(stringLiteral))))
    }
  }

  test("match a literal number") {
    forall(Gen.double) { numberLiteral =>
      val tokens =
        simpleTokens(Token.LiteralNumber(numberLiteral.toString, numberLiteral))
      val result = DefaultParser.parse(tokens)
      expect(result == List(Right(Expression.Literal(numberLiteral))))
    }
  }

  pureTest("match a boolean") {
    val tokens =
      simpleTokens(Token.Keyword.True, Token.Keyword.False)
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(Expression.Literal(true)),
        Right(Expression.Literal(false)),
      )
    )
  }

  pureTest("match a nil value") {
    val tokens =
      simpleTokens(Token.Keyword.Nil)
    val result = DefaultParser.parse(tokens)
    expect(result == List(Right(Expression.Literal(null))))
  }

  test("error for a token that is not handled") {
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

  pureTest("match a unary character starting with !") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Bang,
      Token.Keyword.True,
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Unary(Token.SingleCharacter.Bang, Expression.Literal(true))
        )
      )
    )
  }

  pureTest("match a unary character starting with -") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("123.45", 123.45),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Unary(
            Token.SingleCharacter.Minus,
            Expression.Literal(123.45),
          )
        )
      )
    )
  }

  pureTest("match a unary character with nesting") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Bang,
      Token.SingleCharacter.Minus,
      Token.SingleCharacter.Minus,
      Token.SingleCharacter.Bang,
      Token.Keyword.True,
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Unary(
            Token.SingleCharacter.Bang,
            Expression.Unary(
              Token.SingleCharacter.Minus,
              Expression.Unary(
                Token.SingleCharacter.Minus,
                Expression.Unary(
                  Token.SingleCharacter.Bang,
                  Expression.Literal(true),
                ),
              ),
            ),
          )
        )
      )
    )
  }

  val literalNumberGen: Gen[TokenWithContext]     =
    Gen.posNum[Double].map(n => simpleToken(Token.LiteralNumber(n.toString, n)))
  val literalStringGen: Gen[TokenWithContext]     =
    Gen.alphaNumStr.map(s => simpleToken(Token.LiteralString(s"\"$s\"", s)))
  val literalBooleanGen: Gen[TokenWithContext]    =
    Gen.oneOf(simpleTokens(Token.Keyword.False, Token.Keyword.True))
  val primaryExpressionGen: Gen[TokenWithContext] =
    Gen.oneOf(literalNumberGen, literalStringGen, literalBooleanGen)

  test("parse a valid primary expression") {
    forall(primaryExpressionGen) { primaryToken =>
      val literalValue: Double | String | Boolean =
        primaryToken.token match
          case Token.LiteralNumber(_, l) => l
          case Token.LiteralString(_, l) => l
          case Token.Keyword.True        => true
          case Token.Keyword.False       => false
          case t                         => t.lexeme

      val result = DefaultParser.parse(List(primaryToken))
      expect(result.sequence.isRight)
    }
  }

  val unaryExpressionGen: Gen[List[TokenWithContext]] =
    for
      operator <- Gen.oneOf(
        simpleTokens(Token.SingleCharacter.Bang, Token.SingleCharacter.Minus)
      )
      right    <- Gen.oneOf(
        unaryExpressionGen,
        primaryExpressionGen.map(t => List(t)),
      )
    yield operator +: right

  test("parse a valid unary expression") {
    forall(unaryExpressionGen) { unaryToken =>
      val result = DefaultParser.parse(unaryToken)
      expect(result.sequence.isRight)
    }
  }

end ParserSuite
