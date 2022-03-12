package com.github.pete1232.lox

import weaver.SimpleIOSuite
import Token.*

object ScannerSuite extends SimpleIOSuite:

  pureTest("scan tokens with a single character") {
    import TokenType.SingleCharacter.*
    val bracesResult = DefaultScanner.scan("(){},.-+;/*!=><")
    expect(
      bracesResult.flatMap(_.toSeq) == List(
        SimpleToken(LeftParen, 0),
        SimpleToken(RightParen, 0),
        SimpleToken(LeftBrace, 0),
        SimpleToken(RightBrace, 0),
        SimpleToken(Comma, 0),
        SimpleToken(Dot, 0),
        SimpleToken(Minus, 0),
        SimpleToken(Plus, 0),
        SimpleToken(Semicolon, 0),
        SimpleToken(Slash, 0),
        SimpleToken(Star, 0),
        SimpleToken(Bang, 0),
        SimpleToken(Equal, 0),
        SimpleToken(Greater, 0),
        SimpleToken(Less, 0)
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
