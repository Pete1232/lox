package com.github.pete1232.lox

trait Scanner:
  def scan(source: String): List[Either[ScannerError, Token]]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Either[ScannerError, Token]] =
    scanLoop(source, Nil)

  private def scanLoop(
      remainingInput: String,
      results: List[Either[ScannerError, Token]]
  ): List[Either[ScannerError, Token]] = {
    val nextToken = remainingInput.headOption.map { c =>
      TokenType.SingleCharacter.fromString(c.toString) match
        case None =>
          Left(ScannerError.ParseError(0, "", "Unexpected character."))
        case Some(t) => Right(Token.SimpleToken(t, 0))
    }

    nextToken match
      case None => results // todo this should be an error?
      case Some(lex) if remainingInput.isEmpty => results :+ lex
      case Some(lex) => scanLoop(remainingInput.tail, results :+ lex)
  }
