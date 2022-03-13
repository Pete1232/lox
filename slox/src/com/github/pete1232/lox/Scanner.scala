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
    val nextToken: Option[(Either[ScannerError, Token], Int)] =
      remainingInput.headOption.map { c =>
        TokenType.SingleCharacter.fromString(c.toString) match
          case None =>
            Left(
              ScannerError.ParseError(
                0,
                "",
                "Unexpected character parsing one character token."
              )
            ) -> 1
          case Some(t) if t.isStartOfTwoCharacter =>
            remainingInput.tail.headOption
              .map { c2 =>
                TokenType.TwoCharacter.fromString(
                  c.toString + c2.toString
                ) match
                  case None =>
                    Left(
                      ScannerError.ParseError(
                        0,
                        "",
                        "Unexpected character parsing two character token."
                      )
                    ) -> 2
                  case Some(t2) => Right(Token.SimpleToken(t2, 0)) -> 2
              }
              .getOrElse(Right(Token.SimpleToken(t, 0)) -> 1)
          case Some(t) => Right(Token.SimpleToken(t, 0)) -> 1
      }

    nextToken match
      case None => results // todo this should be an error?
      case Some(lex) if remainingInput.isEmpty => results :+ lex._1
      case Some(lex) => scanLoop(remainingInput.drop(lex._2), results :+ lex._1)
  }
