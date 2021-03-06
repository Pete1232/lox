package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError
import com.github.pete1232.lox.models.{
  Expression,
  ExpressionContext,
  Token,
  TokenContext,
  TokenWithContext,
}
import com.github.pete1232.lox.utils.Showable
import com.github.pete1232.lox.utils.Showable.given

import cats.syntax.all.toTraverseOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.CheckConfig
import weaver.scalacheck.Checkers

object ParserSuite extends SimpleIOSuite with Checkers:

  // large generated expressions can get out of hand quickly
  override val checkConfig = CheckConfig.default.copy(maximumGeneratorSize = 10)

  given ExpressionContext = ExpressionContext(0)

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
      for result <- DefaultParser.parse(tokens)
      yield expect(result == List(Right(Expression.Literal(stringLiteral))))
    }
  }

  test("match a literal number") {
    forall(Gen.double) { numberLiteral =>
      val tokens =
        simpleTokens(Token.LiteralNumber(numberLiteral.toString, numberLiteral))
      for result <- DefaultParser.parse(tokens)
      yield expect(result == List(Right(Expression.Literal(numberLiteral))))
    }
  }

  test("match a boolean") {
    val tokens =
      simpleTokens(Token.Keyword.True, Token.Keyword.False)
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(Expression.Literal(true)),
        Right(Expression.Literal(false)),
      )
    )
  }

  test("match a nil value") {
    val tokens =
      simpleTokens(Token.Keyword.Nil)
    for result <- DefaultParser.parse(tokens)
    yield expect(result == List(Right(Expression.Literal(null))))
  }

  test("error for a token that is not handled") {
    forall(Gen.posNum[Int]) { line =>
      val token  = Token.Keyword.And
      val tokens =
        List(TokenWithContext(token, TokenContext(line)))
      for result <- DefaultParser.parse(tokens)
      yield expect(
        result == List(
          Left(ParserError.UnmatchedTokenError("primary", line, token))
        )
      )
    }
  }

  test("match a unary expression starting with !") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Bang,
      Token.Keyword.True,
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Unary(Token.SingleCharacter.Bang, Expression.Literal(true))
        )
      )
    )
  }

  test("match a unary expression starting with -") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("123.45", 123.45),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("unary should be right associative") {
    val tokens = simpleTokens(
      Token.SingleCharacter.Bang,
      Token.SingleCharacter.Minus,
      Token.SingleCharacter.Minus,
      Token.SingleCharacter.Bang,
      Token.Keyword.True,
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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
    test(s"match a $expressionType expression with ${operator.lexeme}") {
      val tokens = simpleTokens(
        Token.LiteralNumber("120", 120),
        operator,
        Token.LiteralNumber("2", 2),
      )
      for result <- DefaultParser.parse(tokens)
      yield expect(
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

  tokenTest("equality", Token.TwoCharacter.EqualEqual)

  tokenTest("equality", Token.TwoCharacter.BangEqual)

  tokenTest("comma", Token.SingleCharacter.Comma)

  test("match a ternary expression") {
    val tokens =
      simpleTokens(
        Token.Keyword.True,
        Token.SingleCharacter.Question,
        Token.LiteralNumber("1", 1),
        Token.SingleCharacter.Colon,
        Token.LiteralString("\"2\"", "2"),
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Ternary(
            Expression.Literal(true),
            Expression.Literal(1),
            Expression.Literal("2"),
          )
        )
      )
    )
  }

  test("error when a ternary expression is unclosed") {
    val tokens =
      simpleTokens(
        Token.Keyword.True,
        Token.SingleCharacter.Question,
        Token.LiteralNumber("2", 2),
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Left(ParserError.IncompleteConditionalError(0))
      )
    )
  }

  test("match a primary expression within brackets") {
    val tokens =
      simpleTokens(
        Token.SingleCharacter.LeftParen,
        Token.Keyword.True,
        Token.SingleCharacter.RightParen,
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(Expression.Group(Expression.Literal(true)))
      )
    )
  }

  test("match a unary expression within brackets") {
    val tokens =
      simpleTokens(
        Token.SingleCharacter.LeftParen,
        Token.SingleCharacter.Bang,
        Token.Keyword.False,
        Token.SingleCharacter.RightParen,
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Group(
            Expression.Unary(
              Token.SingleCharacter.Bang,
              Expression.Literal(false),
            )
          )
        )
      )
    )
  }

  test("match a binary expression within brackets") {
    val tokens =
      simpleTokens(
        Token.SingleCharacter.LeftParen,
        Token.LiteralNumber("2", 2),
        Token.TwoCharacter.BangEqual,
        Token.LiteralString("\"2\"", "2"),
        Token.SingleCharacter.RightParen,
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Group(
            Expression.Binary(
              Expression.Literal(2),
              Token.TwoCharacter.BangEqual,
              Expression.Literal("2"),
            )
          )
        )
      )
    )
  }

  test("error when an expression is just opened") {
    val tokens =
      simpleTokens(
        Token.SingleCharacter.LeftParen
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Left(ParserError.IncompleteExpression("unary"))
      )
    )
  }

  test("error when an expression is unclosed") {
    val tokens =
      simpleTokens(
        Token.SingleCharacter.LeftParen,
        Token.LiteralNumber("2", 2),
        Token.TwoCharacter.BangEqual,
        Token.LiteralString("\"2\"", "2"),
      )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Left(ParserError.UnclosedGroupError(0))
      )
    )
  }

  test("factors should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.SingleCharacter.Star,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Slash,
      Token.LiteralNumber("15", 15),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("terms should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.SingleCharacter.Plus,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("15", 15),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("comparisons should be left associative") {
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
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("equalities should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.TwoCharacter.EqualEqual,
      Token.LiteralNumber("2", 2),
      Token.TwoCharacter.BangEqual,
      Token.LiteralNumber("15", 15),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Binary(
              Expression.Literal(30),
              Token.TwoCharacter.EqualEqual,
              Expression.Literal(2),
            ),
            Token.TwoCharacter.BangEqual,
            Expression.Literal(15),
          )
        )
      )
    )
  }

  test("conditionals should be right associative") {
    val tokens = simpleTokens(
      Token.Keyword.True,
      Token.SingleCharacter.Question,
      Token.LiteralNumber("1", 1),
      Token.SingleCharacter.Colon,
      Token.Keyword.False,
      Token.SingleCharacter.Question,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Colon,
      Token.LiteralNumber("3", 3),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Ternary(
            Expression.Literal(true),
            Expression.Literal(1),
            Expression.Ternary(
              Expression.Literal(false),
              Expression.Literal(2),
              Expression.Literal(3),
            ),
          )
        )
      )
    )
  }

  test("commas should be left associative") {
    val tokens = simpleTokens(
      Token.LiteralNumber("30", 30),
      Token.SingleCharacter.Comma,
      Token.LiteralNumber("2", 2),
      Token.SingleCharacter.Comma,
      Token.LiteralNumber("15", 15),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Binary(
              Expression.Literal(30),
              Token.SingleCharacter.Comma,
              Expression.Literal(2),
            ),
            Token.SingleCharacter.Comma,
            Expression.Literal(15),
          )
        )
      )
    )
  }

  test("a factor expression should take precedence over a term") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Minus,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Slash,
      Token.LiteralNumber("3", 3),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("a term expression should take precedence over a comparison") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Greater,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Plus,
      Token.LiteralNumber("3", 3),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
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

  test("a comparison expression should take precedence over an equality") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.TwoCharacter.EqualEqual,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Greater,
      Token.LiteralNumber("3", 3),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Literal(7),
            Token.TwoCharacter.EqualEqual,
            Expression.Binary(
              Expression.Literal(6),
              Token.SingleCharacter.Greater,
              Expression.Literal(3),
            ),
          )
        )
      )
    )
  }

  test("a equality expression should take precedence over a comma") {
    val tokens = simpleTokens(
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Comma,
      Token.LiteralNumber("6", 6),
      Token.TwoCharacter.EqualEqual,
      Token.LiteralNumber("3", 3),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Binary(
            Expression.Literal(7),
            Token.SingleCharacter.Comma,
            Expression.Binary(
              Expression.Literal(6),
              Token.TwoCharacter.EqualEqual,
              Expression.Literal(3),
            ),
          )
        )
      )
    )
  }

  test("a equality expression should take precedence over a conditional") {
    val tokens = simpleTokens(
      Token.LiteralNumber("1", 1),
      Token.TwoCharacter.BangEqual,
      Token.LiteralNumber("7", 7),
      Token.SingleCharacter.Question,
      Token.LiteralNumber("6", 6),
      Token.SingleCharacter.Colon,
      Token.LiteralNumber("3", 3),
      Token.TwoCharacter.EqualEqual,
      Token.LiteralNumber("5", 5),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Right(
          Expression.Ternary(
            Expression.Binary(
              Expression.Literal(1),
              Token.TwoCharacter.BangEqual,
              Expression.Literal(7),
            ),
            Expression.Literal(6),
            Expression.Binary(
              Expression.Literal(3),
              Token.TwoCharacter.EqualEqual,
              Expression.Literal(5),
            ),
          )
        )
      )
    )
  }

  test("if there is an error, continue parsing after a semicolon") {
    val tokens = simpleTokens(
      Token.LiteralNumber("1", 1),
      Token.SingleCharacter.Plus,
      Token.SingleCharacter.Comma,
      Token.SingleCharacter.Semicolon,
      Token.LiteralNumber("5", 5),
    )
    for result <- DefaultParser.parse(tokens)
    yield expect(
      result == List(
        Left(
          ParserError
            .UnmatchedTokenError("primary", 0, Token.SingleCharacter.Comma)
        ),
        Right(
          Expression.Literal(5)
        ),
      )
    )
  }

  val statementStartGen: Gen[Token] =
    import Token.Keyword.*
    Gen.oneOf(Class, For, Fun, If, Print, Return, Var, While)

  test(
    "if there is an error, continue parsing from a keyword that begins a statement"
  ) {
    forall(statementStartGen) { statementToken =>
      val tokens = simpleTokens(
        Token.LiteralNumber("1", 1),
        Token.SingleCharacter.Plus,
        Token.SingleCharacter.Comma,
        Token.LiteralNumber("2", 2),
        statementToken,
        Token.LiteralNumber("3", 3),
        Token.SingleCharacter.Semicolon,
        Token.LiteralNumber("5", 5),
      )
      for result <- DefaultParser.parse(tokens)
      yield expect(
        result == List(
          Left(
            ParserError
              .UnmatchedTokenError("primary", 0, Token.SingleCharacter.Comma)
          ),
          // todo eventually this will be parsed properly, for now just expect
          // to skip to the keyword and fail
          Left(
            ParserError
              .UnmatchedTokenError("primary", 0, statementToken)
          ),
          Right(
            Expression.Literal(5)
          ),
        )
      )
    }
  }

  val binaryOperandGen: Gen[TokenWithContext] = Gen
    .oneOf(
      Token.TwoCharacter.BangEqual,
      Token.TwoCharacter.EqualEqual,
      Token.SingleCharacter.Greater,
      Token.TwoCharacter.GreaterEqual,
      Token.SingleCharacter.Less,
      Token.TwoCharacter.LessEqual,
      Token.SingleCharacter.Plus,
      Token.SingleCharacter.Slash,
      Token.SingleCharacter.Star,
    )
    .map(simpleToken)
  test(
    "error and continue when a binary expression is missing a left hand operand"
  ) {
    forall(binaryOperandGen) { operand =>
      val tokens = operand +: simpleTokens(
        Token.LiteralNumber("5", 5),
        Token.LiteralNumber("6", 6),
      )
      for result <- DefaultParser.parse(tokens)
      yield expect(
        result == List(
          Left(
            ParserError.BinaryExpressionNotOpened(0)
          ),
          Right(Expression.Literal(6)),
        )
      )
    }
  }

  // expression  ??? comma ;
  // comma       ??? conditional ( "," conditional )* ;
  // conditional ??? equality ( "?" expression ":" conditional )?
  // equality    ??? comparison ( ( "!=" | "==" ) comparison )* ;
  // comparison  ??? term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  // term        ??? factor ( ( "-" | "+" ) factor )* ;
  // factor      ??? unary ( ( "/" | "*" ) unary )* ;
  // unary       ??? ( "!" | "-" | "--" | "++" ) unary
  //               | postfix ;
  // postfix     ??? primary ( "--" | ++" )* ;
  // primary     ??? NUMBER | STRING | "true" | "false" | "nil"
  //               | "(" expression ")" ;
  //               | ( "!=" | "==" ) equality
  //               | ( ">" | ">=" | "<" | "<=" ) comparison
  //               | ( "+" ) term
  //               | ( "/" | "*" ) factor ;

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
      for result <- DefaultParser.parse(List(primaryToken))
      yield expect(result.sequence.isRight)
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
      for result <- DefaultParser.parse(unaryTokens)
      yield expect(result.sequence.isRight)
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
      for result <- DefaultParser.parse(factorTokens)
      yield expect(result.sequence.isRight)
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
      for result <- DefaultParser.parse(termTokens)
      yield expect(result.sequence.isRight)
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
      for result <- DefaultParser.parse(tokens)
      yield expect(result.sequence.isRight)
    }
  }

  val equalityExpressionGen: Gen[List[TokenWithContext]] =
    val equalityAndFactorGen =
      for
        operator <- Gen.oneOf(
          simpleTokens(
            Token.TwoCharacter.BangEqual,
            Token.TwoCharacter.EqualEqual,
          )
        )
        right    <- comparisonExpressionGen
      yield (operator, right)
    for
      start  <- comparisonExpressionGen
      repeat <- Gen.listOf(equalityAndFactorGen)
    yield repeat.foldLeft(start) { (l, r) =>
      (l :+ r._1) ++ r._2
    }

  test("parse a valid equality expression") {
    forall(equalityExpressionGen) { tokens =>
      for result <- DefaultParser.parse(tokens)
      yield expect(result.sequence.isRight)
    }
  }

  val commaExpressionGen: Gen[List[TokenWithContext]] =
    val commaAndEqualityGen =
      for right <- equalityExpressionGen
      yield (simpleToken(Token.SingleCharacter.Comma), right)
    for
      start  <- comparisonExpressionGen
      repeat <- Gen.listOf(commaAndEqualityGen)
    yield repeat.foldLeft(start) { (l, r) =>
      (l :+ r._1) ++ r._2
    }

  test("parse a valid comma expression") {
    forall(commaExpressionGen) { tokens =>
      for result <- DefaultParser.parse(tokens)
      yield expect(result.sequence.isRight)
    }
  }

  val expressionGen: Gen[List[TokenWithContext]] = equalityExpressionGen

  val primaryGroupExpressionGen: Gen[List[TokenWithContext]] =
    expressionGen.map(expr =>
      simpleToken(Token.SingleCharacter.LeftParen) +: expr :+ simpleToken(
        Token.SingleCharacter.RightParen
      )
    )

  test("parse a valid primary group expression") {
    forall(primaryGroupExpressionGen) { tokens =>
      for result <- DefaultParser.parse(tokens)
      yield expect(result.sequence.isRight)
    }
  }

  val conditionalExpressionGen: Gen[List[TokenWithContext]] =
    val expressionAndConditionalGen =
      for
        expression  <- expressionGen
        conditional <- conditionalExpressionGen
      yield ((simpleToken(
        Token.SingleCharacter.Question
      ) +: expression) :+ simpleToken(
        Token.SingleCharacter.Colon
      )) ++ conditional
    for
      left  <- equalityExpressionGen
      right <- Gen.option(expressionAndConditionalGen)
    yield left ++ right.getOrElse(Nil)

  test("parse a valid conditional expression") {
    forall(conditionalExpressionGen) { tokens =>
      for result <- DefaultParser.parse(tokens)
      yield expect(result.sequence.isRight)
    }
  }

end ParserSuite
