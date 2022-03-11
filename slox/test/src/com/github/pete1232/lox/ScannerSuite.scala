package com.github.pete1232.lox

import weaver.SimpleIOSuite

object ScannerSuite extends SimpleIOSuite:

  pureTest("scan tokens with a single character") {
    import TokenType.SingleCharacter.*
    val bracesResult = DefaultScanner.scan("()")
    expect(
      bracesResult == List(
        Right(Token(LeftParen, "(", null, 0)),
        Right(Token(RightParen, ")", null, 0))
      )
    )
  }

  pureTest("report an error for unknown characters") {
    import TokenType.SingleCharacter.*
    val hashResult = DefaultScanner.scan("+#-")
    expect(
      hashResult == List(
        Right(Token(Plus, "+", null, 0)),
        Left(ScannerError.ParseError(0, "", "Unexpected character.")),
        Right(Token(Minus, "-", null, 0))
      )
    )
  }
