package com.github.pete1232.lox

import com.github.pete1232.lox.Token.*
import com.github.pete1232.lox.errors.ScannerError
import com.github.pete1232.lox.utils.LoggerBootstrap
import com.github.pete1232.lox.utils.Showable.given

import scala.util.hashing.Hashing.Default

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ScannerSuite extends SimpleIOSuite with Checkers:

  val singleCharacterTokenGen: Gen[Token] =
    Gen.oneOf(Token.SingleCharacter.values)

  val twoCharacterTokenGen: Gen[Token] =
    Gen.oneOf(Token.TwoCharacter.values)

  val scanner = DefaultScanner

  test("scan tokens with a single character") {
    forall(singleCharacterTokenGen) { token =>
      for result <- scanner.scan(token.lexeme)
      yield expect(
        result.flatMap(_.toSeq) == List(
          TokenWithContext(token, TokenContext(0))
        )
      )
    }
  }

  test("scan tokens with two characters") {
    forall(twoCharacterTokenGen) { token =>
      for result <- scanner.scan(token.lexeme)
      yield expect(
        result.flatMap(_.toSeq) == List(
          TokenWithContext(token, TokenContext(0))
        )
      )
    }
  }

  test("report an error for an unknown character") {
    for hashResult <- scanner.scan("#")
    yield expect(
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

  test("report an error for an unknown character after another token") {
    for hashResult <- scanner.scan("!#")
    yield expect(
      hashResult == List(
        Right(TokenWithContext(Token.SingleCharacter.Bang, TokenContext(0))),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "#",
          )
        ),
      )
    )
  }

  test("ignore everything on a comment line") {
    forall(Gen.alphaStr) { s =>
      for commentResult <- scanner.scan("//" + s)
      yield expect(commentResult == Nil)
    }
  }

  test(s"ignore whitespace between tokens") {
    import Token.SingleCharacter.*
    for result <- scanner.scan("\r(\t= = )\n{ } ;")
    yield expect(
      result.map(_.map(_.token)) == List(
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

  test("report the line correctly") {
    import Token.SingleCharacter.*
    for result <- scanner.scan("+\n* \n / // this is a comment \n- #")
    yield expect(
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

  test("parse single character tokens with no spacing") {
    import Token.SingleCharacter.*
    for result <- scanner.scan("*+-/")
    yield expect(
      result == List(
        Right(TokenWithContext(Star, TokenContext(0))),
        Right(TokenWithContext(Plus, TokenContext(0))),
        Right(TokenWithContext(Minus, TokenContext(0))),
        Right(TokenWithContext(Slash, TokenContext(0))),
      )
    )
  }

  test(
    "error if a valid single character token is followed by an invalid one"
  ) {
    import Token.SingleCharacter.*
    for result <- scanner.scan("+% /@- !£")
    yield expect(
      result == List(
        Right(TokenWithContext(Plus, TokenContext(0))),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "%",
          )
        ),
        Right(
          TokenWithContext(Token.SingleCharacter.Slash, TokenContext(0))
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "@-",
          )
        ),
        Right(
          TokenWithContext(Token.SingleCharacter.Bang, TokenContext(0))
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "£",
          )
        ),
      )
    )
  }

  test("parse two character tokens with no spacing") {
    import Token.TwoCharacter.*
    for result <- scanner.scan("==!=>=")
    yield expect(
      result == List(
        Right(TokenWithContext(EqualEqual, TokenContext(0))),
        Right(TokenWithContext(BangEqual, TokenContext(0))),
        Right(TokenWithContext(GreaterEqual, TokenContext(0))),
      )
    )
  }

  test("error if a two character token is followed by an invalid one") {
    import Token.SingleCharacter.*
    import Token.TwoCharacter.*

    for result <- scanner.scan("==* ==^;&")
    yield expect(
      result == List(
        Right(TokenWithContext(EqualEqual, TokenContext(0))),
        Right(TokenWithContext(Star, TokenContext(0))),
        Right(TokenWithContext(EqualEqual, TokenContext(0))),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "^;&",
          )
        ),
      )
    )
  }

  test("parse a string literal") {
    forall(Gen.alphaStr) { str =>
      val inputString = "\"" + str + "\""
      for result <- scanner.scan(inputString)
      yield expect(
        result.map(_.map(_.token)) == List(
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

  test("parse a string literal with an escaped quote") {
    val inputString = "\"" + "abc123\\\"" + "\""
    for result <- scanner.scan(inputString)
    yield expect(
      result.map(_.map(_.token)) == List(
        Right(
          LiteralString(
            inputString,
            "abc123\"",
          )
        )
      )
    )
  }

  test("error when a string literal doesn't have a closing quote") {
    for result <- scanner.scan("\"" + "abc123")
    yield expect(
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

  test(
    "error when a string literal doesn't have a closing quote on the same line"
  ) {
    for result <- scanner.scan("\"" + "abc123" + "\n" + "\"")
    yield expect(
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
      for result <- scanner
          .scan(num.toString + " \"another token\"")
      yield expect(
        result.map(_.map(_.token)) == List(
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
      for result <- scanner
          .scan(num.toString + "\n\"another token\"")
      yield expect(
        result.map(_.map(_.token)) == List(
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

  test("error when a numeric isn't closed properly by a space") {
    for result <- scanner.scan("123a")
    yield expect(
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
      for result <- scanner.scan(str)
      yield expect(
        result.map(_.map(_.token)) == List(
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
    val keywordGen: Gen[Token] = Gen.oneOf(
      Token.Keyword.values
    )

    forall(keywordGen) { k =>
      for result <- scanner.scan(k.lexeme)
      yield expect(
        result.map(_.map(_.token)) == List(
          Right(
            k
          )
        )
      )
    }
  }

  test("error if the token does not exist and isn't a valid identifier") {
    for result <- scanner.scan("@ 1t\t_tes\rtest!")
    yield expect(
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

  test("parse strings up to a space on errors") {
    import Token.SingleCharacter.*
    import Token.TwoCharacter.*

    for result <-
        scanner.scan("_test !@1 *a* !=a, 123a5 123.4.5 test!id var~if")
    yield expect(
      result == List(
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "_test",
          )
        ),
        Right(
          TokenWithContext(Token.SingleCharacter.Bang, TokenContext(0))
        ),
        Left(
          ScannerError.InvalidFirstCharacter(
            0,
            "@1",
          )
        ),
        Right(
          TokenWithContext(Star, TokenContext(0))
        ),
        Left(
          ScannerError.LiteralIdentifierBadCharacter(
            0,
            "a*",
          )
        ),
        Right(
          TokenWithContext(BangEqual, TokenContext(0))
        ),
        Left(
          ScannerError.LiteralIdentifierBadCharacter(
            0,
            "a,",
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

  test("allow single-line comments with /**/ syntax") {
    for result <- scanner
        .scan("var a = /*a single line / * comment*/ 1")
    yield expect(
      result.map(_.map(_.token)) == List(
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

  test("allow multi-line comments") {
    for result <- scanner.scan(
        "var a = /*a \n multi \n line / \n * comment*/ 1"
      )
    yield expect(
      result == List(
        Right(TokenWithContext(Token.Keyword.Var, TokenContext(0))),
        Right(TokenWithContext(LiteralIdentifier("a", "a"), TokenContext(0))),
        Right(TokenWithContext(Token.SingleCharacter.Equal, TokenContext(0))),
        Right(TokenWithContext(LiteralNumber("1", 1), TokenContext(3))),
      )
    )
  }

  test("error on unclosed comments") {
    for result <- scanner.scan("/* test")
    yield expect(
      result == List(
        Left(
          ScannerError.UnclosedComment(0, "/* test")
        )
      )
    )
  }

  test(
    "parse a single character token that can also start a two character"
  ) {
    for result <- scanner.scan("1<3")
    yield expect(
      result == List(
        Right(TokenWithContext(Token.LiteralNumber("1", 1), TokenContext(0))),
        Right(TokenWithContext(Token.SingleCharacter.Less, TokenContext(0))),
        Right(TokenWithContext(Token.LiteralNumber("3", 3), TokenContext(0))),
      )
    )
  }

  test("allow simple numeric expressions with no spacing") {
    for result <- scanner.scan("1!=2<3")
    yield expect(
      result == List(
        Right(TokenWithContext(Token.LiteralNumber("1", 1), TokenContext(0))),
        Right(TokenWithContext(Token.TwoCharacter.BangEqual, TokenContext(0))),
        Right(TokenWithContext(Token.LiteralNumber("2", 2), TokenContext(0))),
        Right(TokenWithContext(Token.SingleCharacter.Less, TokenContext(0))),
        Right(TokenWithContext(Token.LiteralNumber("3", 3), TokenContext(0))),
      )
    )
  }

  test("do not allow a numeric literal with an alpha-numeric character") {
    for result <- scanner.scan("123abc")
    yield expect(
      result == List(
        Left(ScannerError.LiteralNumberBadCharacter(0, "123abc"))
      )
    )
  }

  test("allow simple string expressions with no spacing") {
    for result <- scanner.scan("\"a\"!=\"b\"")
    yield expect(
      result == List(
        Right(
          TokenWithContext(Token.LiteralString("\"a\"", "a"), TokenContext(0))
        ),
        Right(TokenWithContext(Token.TwoCharacter.BangEqual, TokenContext(0))),
        Right(
          TokenWithContext(Token.LiteralString("\"b\"", "b"), TokenContext(0))
        ),
      )
    )
  }

end ScannerSuite
