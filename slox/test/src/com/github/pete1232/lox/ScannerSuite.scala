package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*
import org.scalacheck.Gen
import weaver.scalacheck.Checkers

object ScannerSuite extends SimpleIOSuite with Checkers:

  val singleCharacterTokenGen: Gen[TokenType.SingleCharacter] =
    Gen.oneOf(TokenType.SingleCharacter.values)

  val twoCharacterTokenGen: Gen[TokenType.TwoCharacter] =
    Gen.oneOf(TokenType.TwoCharacter.values)

  test("scan tokens with a single character") {
    forall(singleCharacterTokenGen) { token =>
      val result = DefaultScanner.scan(token.lexeme).flatMap(_.toSeq)
      expect(result == List(SimpleToken(token, 0)))
    }
  }

  test("scan tokens with two characters") {
    forall(twoCharacterTokenGen) { token =>
      val result = DefaultScanner.scan(token.lexeme).flatMap(_.toSeq)
      expect(result == List(SimpleToken(token, 0)))
    }
  }

  pureTest("report an error for an unknown character") {
    val hashResult = DefaultScanner.scan("#")
    expect(
      hashResult == List(
        Left(
          ScannerError.ParseError(
            0,
            "",
            "Unexpected character parsing one character token.",
            "#"
          )
        )
      )
    )
  }

  pureTest("report an error for an unknown character after another token") {
    val hashResult = DefaultScanner.scan("!#")
    expect(
      hashResult == List(
        Left(
          ScannerError.ParseError(
            0,
            "",
            "Unexpected character parsing two character token.",
            "!#"
          )
        )
      )
    )
  }

  test("ignore everything on a comment line") {
    forall(Gen.alphaStr) { s =>
      val commentResult = DefaultScanner.scan("//" + s)
      expect(commentResult == Nil)
    }
  }

  pureTest("ignore whitespace between tokens") {
    import TokenType.SingleCharacter.*
    val result = DefaultScanner.scan("\r(\t= = )\n{} ;")
    expect(
      result.map(_.map(_.tokenType)) == List(
        Right(LeftParen),
        Right(Equal),
        Right(Equal),
        Right(RightParen),
        Right(LeftBrace),
        Right(RightBrace),
        Right(Semicolon)
      )
    )
  }

  pureTest("report the line correctly") {
    import TokenType.SingleCharacter.*
    val result = DefaultScanner.scan("+\n* \n / // this is a comment \n- #")
    expect(
      result == List(
        Right(SimpleToken(Plus, 0)),
        Right(SimpleToken(Star, 1)),
        Right(SimpleToken(Slash, 2)),
        Right(SimpleToken(Minus, 3)),
        Left(
          ScannerError.ParseError(
            3,
            "",
            "Unexpected character parsing one character token.",
            "#"
          )
        )
      )
    )
  }
