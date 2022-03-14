package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*
import org.scalacheck.Gen
import weaver.scalacheck.Checkers
import scala.util.hashing.Hashing.Default

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
          ScannerError.InvalidFirstCharacter(
            0,
            "#",
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
          ScannerError.InvalidSecondCharacter(
            0,
            "!#",
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
    val result = DefaultScanner.scan("\r(\t= = )\n{ } ;")
    expect(
      result.map(_.map(_.tokenType)) == List(
        Right(LeftParen),
        Right(Equal),
        Right(Equal),
        Right(RightParen),
        Right(LeftBrace),
        Right(RightBrace),
        Right(Semicolon),
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
          ScannerError.InvalidFirstCharacter(
            3,
            "#",
          )
        ),
      )
    )
  }

  pureTest("error if the token does not exist") {
    val result = DefaultScanner.scan("t te\ttes\ntest\rtest!")

    expect(
      result == List(
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "t",
          )
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "te",
          )
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "tes",
          )
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            1,
            "test",
          )
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            1,
            "test!",
          )
        ),
      )
    )
  }

  pureTest("error if no space is left between valid single character tokens") {
    import TokenType.SingleCharacter.*
    import TokenType.TwoCharacter.*

    val result = DefaultScanner.scan("*/ +% /*- !£")

    expect(
      result == List(
        Left(
          ScannerError.ValidOneCharacterNoWhitespace(
            0,
            "*/",
          )
        ),
        Left(
          ScannerError.ValidOneCharacterNoWhitespace(
            0,
            "+%",
          )
        ),
        Left(
          ScannerError.InvalidSecondCharacter(
            0,
            "/*-",
          )
        ),
        Left(
          ScannerError.InvalidSecondCharacter(
            0,
            "!£",
          )
        ),
      )
    )
  }

  pureTest("error if no space is left between valid two character tokens") {
    import TokenType.SingleCharacter.*
    import TokenType.TwoCharacter.*

    val result = DefaultScanner.scan("==* ==^;&")

    expect(
      result == List(
        Left(
          ScannerError.ValidTwoCharacterNoWhitespace(
            0,
            "==*",
          )
        ),
        Left(
          ScannerError.ValidTwoCharacterNoWhitespace(
            0,
            "==^;&",
          )
        ),
      )
    )
  }

  test("parse a string literal") {
    forall(Gen.alphaStr) { str =>
      val stringObject = "\"" + str + "\""
      val result       = DefaultScanner.scan(stringObject)
      expect(
        result == List(
          Right(
            LiteralToken(
              TokenType.Literal.StringLiteral,
              str,
              stringObject,
              0,
            )
          )
        )
      )
    }
  }

  pureTest("parse a string literal with an escaped quote") {
    val stringObject = "\"" + "abc123\\\"" + "\""
    val result       = DefaultScanner.scan(stringObject)
    expect(
      result == List(
        Right(
          LiteralToken(
            TokenType.Literal.StringLiteral,
            "abc123\\\"",
            stringObject,
            0,
          )
        )
      )
    )
  }

  pureTest("error when a string literal doesn't have a closing quote") {
    val result = DefaultScanner.scan("\"" + "abc123")
    expect(
      result == List(
        Left(
          ScannerError.LiteralStringNotClosed(
            0,
            "\"abc123",
          )
        )
      )
    )
  }

  pureTest(
    "error when a string literal doesn't have a closing quote on the same line"
  ) {
    val result = DefaultScanner.scan("\"" + "abc123" + "\n" + "\"")
    println(result)
    expect(
      result == List(
        Left(
          ScannerError.LiteralStringNotClosed(
            0,
            "\"abc123",
          )
        ),
        Left(
          ScannerError.LiteralStringNotClosed(
            1,
            "\"",
          )
        ),
      )
    )
  }

end ScannerSuite
