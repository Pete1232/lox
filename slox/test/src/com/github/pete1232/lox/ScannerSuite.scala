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
        Left(ScannerError.ParseError(0, "", "Unexpected character parsing one character token."))
      )
    )
  }

  pureTest("report an error for an unknown character after another token") {
    val hashResult = DefaultScanner.scan("!#")
    expect(
      hashResult == List(
        Left(ScannerError.ParseError(0, "", "Unexpected character parsing two character token."))
      )
    )
  }
