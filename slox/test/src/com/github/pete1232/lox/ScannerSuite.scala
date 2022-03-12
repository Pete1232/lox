package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*

object ScannerSuite extends SimpleIOSuite:

  pureTest("scan tokens with a single character") {
    import TokenType.SingleCharacter.*
    val bracesResult = DefaultScanner.scan("()")
    expect(
      bracesResult == List(
        Right(SimpleToken(LeftParen, 0)),
        Right(SimpleToken(RightParen, 0))
      )
    )
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
