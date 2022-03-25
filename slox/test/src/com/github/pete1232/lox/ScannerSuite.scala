package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*
import org.scalacheck.Gen
import weaver.scalacheck.Checkers
import scala.util.hashing.Hashing.Default

object ScannerSuite extends SimpleIOSuite with Checkers:

  val singleCharacterTokenGen: Gen[Token.SingleCharacter] =
    Gen.oneOf(Token.SingleCharacter.values)

  val twoCharacterTokenGen: Gen[Token.TwoCharacter] =
    Gen.oneOf(Token.TwoCharacter.values)

  test("scan tokens with a single character") {
    forall(singleCharacterTokenGen) { token =>
      val result = DefaultScanner.scan(token.lexeme).flatMap(_.toSeq)
      expect(result == List(TokenWithContext(token, TokenContext(0))))
    }
  }

  test("scan tokens with two characters") {
    forall(twoCharacterTokenGen) { token =>
      val result = DefaultScanner.scan(token.lexeme).flatMap(_.toSeq)
      expect(result == List(TokenWithContext(token, TokenContext(0))))
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
    import Token.SingleCharacter.*
    val result = DefaultScanner.scan("\r(\t= = )\n{ } ;").map(_.map(_.token))
    expect(
      result == List(
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
    import Token.SingleCharacter.*
    val result = DefaultScanner.scan("+\n* \n / // this is a comment \n- #")
    expect(
      result == List(
        Right(TokenWithContext(Plus, TokenContext(0))),
        Right(TokenWithContext(Star, TokenContext(1))),
        Right(TokenWithContext(Slash, TokenContext(2))),
        Right(TokenWithContext(Minus, TokenContext(3))),
        Left(
          ScannerError.InvalidFirstCharacter(
            3,
            "#",
          )
        ),
      )
    )
  }

  pureTest("error if no space is left between valid single character tokens") {
    import Token.SingleCharacter.*
    import Token.TwoCharacter.*

    val result = DefaultScanner.scan("*/ +% /@- !£")

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
            "/@-",
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
    import Token.SingleCharacter.*
    import Token.TwoCharacter.*

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
      val inputString = "\"" + str + "\""
      val result      = DefaultScanner.scan(inputString).map(_.map(_.token))
      expect(
        result == List(
          Right(
            LiteralString(
              inputString,
              str,
            )
          )
        )
      )
    }
  }

  pureTest("parse a string literal with an escaped quote") {
    val inputString = "\"" + "abc123\\\"" + "\""
    val result      = DefaultScanner.scan(inputString).map(_.map(_.token))
    expect(
      result == List(
        Right(
          LiteralString(
            inputString,
            "abc123\"",
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

  test("parse a valid positive long as a numeric literal") {
    forall(Gen.posNum[Long]) { num =>
      val result = DefaultScanner
        .scan(num.toString + " \"another token\"")
        .map(_.map(_.token))
      expect(
        result == List(
          Right(
            LiteralNumber(
              num.toString,
              num,
            )
          ),
          Right(
            LiteralString(
              "\"another token\"",
              "another token",
            )
          ),
        )
      )
    }
  }

  test("parse a valid positive double as a numeric literal") {
    forall(Gen.posNum[Double]) { num =>
      val result = DefaultScanner
        .scan(num.toString + "\n\"another token\"")
        .map(_.map(_.token))
      expect(
        result == List(
          Right(
            LiteralNumber(
              num.toString,
              num,
            )
          ),
          Right(
            LiteralString(
              "\"another token\"",
              "another token",
            )
          ),
        )
      )
    }
  }

  pureTest("error when a numeric isn't closed properly by a space") {
    val result = DefaultScanner.scan("123a")
    expect(
      result == List(
        Left(
          ScannerError.LiteralNumberBadCharacter(
            0,
            "123a",
          )
        )
      )
    )
  }

  test("parse any valid string of characters as an identifier") {
    val identifierGen: Gen[String] =
      for
        c <- Gen.alphaChar
        s <- Gen.stringOf(
          Gen.oneOf(
            (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z') :+ ('_')).toArray
          )
        )
      yield c + s

    forall(
      identifierGen.filterNot(Token.Keyword.values.map(_.lexeme).contains)
    ) { str =>
      val result = DefaultScanner.scan(str).map(_.map(_.token))
      expect(
        result == List(
          Right(
            LiteralIdentifier(
              str,
              str,
            )
          )
        )
      )
    }
  }

  test("parse any valid keywords") {
    val keywordGen: Gen[Token.Keyword] = Gen.oneOf(
      Token.Keyword.values
    )

    forall(keywordGen) { k =>
      val result = DefaultScanner.scan(k.lexeme).map(_.map(_.token))
      expect(
        result == List(
          Right(
            k
          )
        )
      )
    }
  }

  pureTest("error if the token does not exist and isn't a valid identifier") {
    val result = DefaultScanner.scan("@ 1t\t_tes\rtest!")

    expect(
      result == List(
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "@",
          )
        ),
        Left(
          ScannerError.LiteralNumberBadCharacter(
            0,
            "1t",
          )
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "_tes",
          )
        ),
        Left(
          ScannerError.LiteralIdentifierBadCharacter(
            0,
            "test!",
          )
        ),
      )
    )
  }

  pureTest("parse strings up to a space on errors") {
    val result =
      DefaultScanner.scan("_test !a; *a* !=a, 123a5 123.4.5 test!id var~if")

    expect(
      result == List(
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "_test",
          )
        ),
        Left(
          ScannerError.InvalidSecondCharacter(
            0,
            "!a;",
          )
        ),
        Left(
          ScannerError.ValidOneCharacterNoWhitespace(
            0,
            "*a*",
          )
        ),
        Left(
          ScannerError.ValidTwoCharacterNoWhitespace(
            0,
            "!=a,",
          )
        ),
        Left(
          ScannerError.LiteralNumberBadCharacter(
            0,
            "123a5",
          )
        ),
        Left(
          ScannerError.LiteralNumberTwoPoints(
            0,
            "123.4.5",
          )
        ),
        Left(
          ScannerError.LiteralIdentifierBadCharacter(
            0,
            "test!id",
          )
        ),
        Left(
          ScannerError.LiteralIdentifierBadCharacter(
            0,
            "var~if",
          )
        ),
      )
    )
  }

  pureTest("allow single-line comments with /**/ syntax") {
    val result = DefaultScanner
      .scan("var a = /*a single line / * comment*/ 1")
      .map(_.map(_.token))

    expect(
      result == List(
        Right(
          Token.Keyword.Var
        ),
        Right(
          LiteralIdentifier("a", "a")
        ),
        Right(Token.SingleCharacter.Equal),
        Right(LiteralNumber("1", 1)),
      )
    )
  }

  pureTest("allow multi-line comments") {
    val result = DefaultScanner.scan(
      "var a = /*a \n multi \n line / \n * comment*/ 1"
    )
    expect(
      result == List(
        Right(TokenWithContext(Token.Keyword.Var, TokenContext(0))),
        Right(TokenWithContext(LiteralIdentifier("a", "a"), TokenContext(0))),
        Right(TokenWithContext(Token.SingleCharacter.Equal, TokenContext(0))),
        Right(TokenWithContext(LiteralNumber("1", 1), TokenContext(3))),
      )
    )
  }

  pureTest("error on unclosed comments") {
    val result = DefaultScanner.scan("/* test")
    expect(
      result == List(
        Left(
          ScannerError.UnclosedComment(0, "/* test")
        )
      )
    )
  }

end ScannerSuite
