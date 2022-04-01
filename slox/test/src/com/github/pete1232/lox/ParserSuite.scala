package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

import cats.implicits.*
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.CheckConfig
import weaver.scalacheck.Checkers

object ParserSuite extends SimpleIOSuite with Checkers:

  // large generated expressions can get out of hand quickly
  override val checkConfig = CheckConfig.default.copy(maximumGeneratorSize = 10)

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

  pureTest("match a unary expression starting with !") {
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

  pureTest("match a unary expression starting with -") {
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

  pureTest("unary should be right associative") {
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

  private def tokenTest(
      expressionType: String,
      operator: Expression.BinaryOperator,
  ) =
    pureTest(s"match a $expressionType expression with ${operator.lexeme}") {
      val tokens = simpleTokens(
        Token.LiteralNumber("120", 120),
        operator,
        Token.LiteralNumber("2", 2),
      )
      val result = DefaultParser.parse(tokens)
      expect(
        result == List(
          Right(
            Expression.Binary(
              Expression.Literal(120),
              operator,
              Expression.Literal(2),
            )
          )
        )
      )
    }

  tokenTest("factor", Token.SingleCharacter.Slash)

  tokenTest("factor", Token.SingleCharacter.Star)

  tokenTest("term", Token.SingleCharacter.Minus)

  tokenTest("term", Token.SingleCharacter.Plus)

  tokenTest("comparison", Token.SingleCharacter.Greater)

  tokenTest("comparison", Token.TwoCharacter.GreaterEqual)

  tokenTest("comparison", Token.SingleCharacter.Less)

  tokenTest("comparison", Token.TwoCharacter.LessEqual)

  pureTest("factors should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.SingleCharacter.Star,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Slash,
      Token.LiteralNumber("15", 15),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Binary(
              Expression.Literal(30),
              Token.SingleCharacter.Star,
              Expression.Literal(2),
            ),
            Token.SingleCharacter.Slash,
            Expression.Literal(15),
          )
        )
      )
    )
  }

  pureTest("terms should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.SingleCharacter.Plus,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("15", 15),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Binary(
              Expression.Literal(30),
              Token.SingleCharacter.Plus,
              Expression.Literal(2),
            ),
            Token.SingleCharacter.Minus,
            Expression.Literal(15),
          )
        )
      )
    )
  }

  pureTest("comparisons should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("1", 1),
      Token.SingleCharacter.Greater,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Less,
      Token.LiteralNumber("3", 3),
      Token.TwoCharacter.GreaterEqual,
      Token.LiteralNumber("4", 4),
      Token.TwoCharacter.LessEqual,
      Token.LiteralNumber("5", 5),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Binary(
              Expression.Binary(
                Expression.Binary(
                  Expression.Literal(1),
                  Token.SingleCharacter.Greater,
                  Expression.Literal(2),
                ),
                Token.SingleCharacter.Less,
                Expression.Literal(3),
              ),
              Token.TwoCharacter.GreaterEqual,
              Expression.Literal(4),
            ),
            Token.TwoCharacter.LessEqual,
            Expression.Literal(5),
          )
        )
      )
    )
  }

  pureTest("a factor expression should take precedence over a term") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Slash,
      Token.LiteralNumber("3", 3),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Literal(7),
            Token.SingleCharacter.Minus,
            Expression.Binary(
              Expression.Literal(6),
              Token.SingleCharacter.Slash,
              Expression.Literal(3),
            ),
          )
        )
      )
    )
  }

  pureTest("a term expression should take precedence over a comparison") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Greater,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Plus,
      Token.LiteralNumber("3", 3),
    )
    val result = DefaultParser.parse(tokens)
    expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Literal(7),
            Token.SingleCharacter.Greater,
            Expression.Binary(
              Expression.Literal(6),
              Token.SingleCharacter.Plus,
              Expression.Literal(3),
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
    forall(unaryExpressionGen) { unaryTokens =>
      val result = DefaultParser.parse(unaryTokens)
      expect(result.sequence.isRight)
    }
  }

  val factorExpressionGen: Gen[List[TokenWithContext]] =
    val operatorAndUnaryGen =
      for
        operator <- Gen.oneOf(
          simpleTokens(Token.SingleCharacter.Slash, Token.SingleCharacter.Star)
        )
        right    <- unaryExpressionGen
      yield (operator, right)
    for
      start  <- unaryExpressionGen
      repeat <- Gen.listOf(operatorAndUnaryGen)
    yield repeat.foldLeft(start) { (l, r) =>
      (l :+ r._1) ++ r._2
    }

  test("parse a valid factor expression") {
    forall(factorExpressionGen) { factorTokens =>
      val result = DefaultParser.parse(factorTokens)
      expect(result.sequence.isRight)
    }
  }

  val termExpressionGen: Gen[List[TokenWithContext]] =
    val operatorAndFactorGen =
      for
        operator <- Gen.oneOf(
          simpleTokens(Token.SingleCharacter.Plus, Token.SingleCharacter.Minus)
        )
        right    <- factorExpressionGen
      yield (operator, right)
    for
      start  <- factorExpressionGen
      repeat <- Gen.listOf(operatorAndFactorGen)
    yield repeat.foldLeft(start) { (l, r) =>
      (l :+ r._1) ++ r._2
    }

  test("parse a valid term expression") {
    forall(termExpressionGen) { termTokens =>
      val result = DefaultParser.parse(termTokens)
      expect(result.sequence.isRight)
    }
  }

  val comparisonExpressionGen: Gen[List[TokenWithContext]] =
    val comparisonAndFactorGen =
      for
        operator <- Gen.oneOf(
          simpleTokens(
            Token.SingleCharacter.Greater,
            Token.TwoCharacter.GreaterEqual,
            Token.SingleCharacter.Less,
            Token.TwoCharacter.LessEqual,
          )
        )
        right    <- termExpressionGen
      yield (operator, right)
    for
      start  <- termExpressionGen
      repeat <- Gen.listOf(comparisonAndFactorGen)
    yield repeat.foldLeft(start) { (l, r) =>
      (l :+ r._1) ++ r._2
    }

  test("parse a valid comparison expression") {
    forall(comparisonExpressionGen) { tokens =>
      val result = DefaultParser.parse(tokens)
      expect(result.sequence.isRight)
    }
  }

end ParserSuite
