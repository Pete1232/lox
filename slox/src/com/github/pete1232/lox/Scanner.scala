package com.github.pete1232.lox

import com.github.pete1232.lox.TokenType.TwoCharacter

trait Scanner:
  def scan(source: String): List[Either[ScannerError, Token]]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Either[ScannerError, Token]] =
    scanLoop(source, Nil)

  private def scanLoop(
      remainingInput: String,
      results: List[Either[ScannerError, Token]]
  ): List[Either[ScannerError, Token]] = {

    val firstCharacter = remainingInput.headOption
    lazy val secondCharacter = remainingInput.tail.headOption

    // todo line number
    def singleCharacterResult(char: Char) = Some(
      TokenType.SingleCharacter
        .fromString(char.toString)
        .map(tokenType => Token.SimpleToken(tokenType, 0))
        .toRight(
          ScannerError.ParseError(
            0,
            "",
            "Unexpected character parsing one character token.",
            char.toString
          )
        )
    )

    val nextToken: Option[Either[ScannerError, Token]] = firstCharacter match
      case None => None
      case Some(char) =>
        char match
          case '\n' => None // todo needs to move onto a new line
          case ' '  => None
          case _ if TwoCharacter.entrypoints.contains(char) =>
            secondCharacter match
              case None => singleCharacterResult(char)
              case Some(char2) =>
                char2 match
                  case '\n' =>
                    singleCharacterResult(
                      char
                    ) // todo needs to move onto a new line
                  case ' ' => singleCharacterResult(char)
                  case _ =>
                    val lexeme = char.toString + char2.toString
                    Some(
                      TwoCharacter
                        .fromString(lexeme)
                        .map(tokenType => Token.SimpleToken(tokenType, 0))
                        .toRight(
                          ScannerError.ParseError(
                            0,
                            "",
                            "Unexpected character parsing two character token.",
                            lexeme
                          )
                        )
                    )
          case _ => singleCharacterResult(char)

    nextToken match
      case None => results // todo this should be an error?
      case Some(lex) if remainingInput.isEmpty => results :+ lex
      case Some(lex) =>
        scanLoop(
          remainingInput.drop(lex.fold(_.lexeme.length, _.length)),
          results :+ lex
        )
  }
