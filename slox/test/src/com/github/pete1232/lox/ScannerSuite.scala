package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*
import org.scalacheck.Gen
import weaver.scalacheck.Checkers

object ScannerSuite extends SimpleIOSuite with Checkers:

  val singleCharacterTokenGen: Gen[TokenType.SingleCharacter] =
    Gen.oneOf(TokenType.SingleCharacter.values)

  test("scan tokens with a single character") {
    forall(singleCharacterTokenGen) { token =>
      val result = DefaultScanner.scan(token.lexeme).flatMap(_.toSeq)
      expect(result == List(SimpleToken(token, 0)))
    }
  }

  pureTest("report an error for unknown characters") {
    import TokenType.SingleCharacter.*
    val hashResult = DefaultScanner.scan("+#-")
    expect(
      hashResult == List(
        Right(SimpleToken(Plus, 0)),
        Left(ScannerError.ParseError(0, "", "Unexpected character.")),
        Right(SimpleToken(Minus, 0))
      )
    )
  }
