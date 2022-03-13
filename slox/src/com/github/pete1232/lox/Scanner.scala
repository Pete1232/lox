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
    import ScannerResult.*

    val firstCharacter = remainingInput.headOption
    lazy val secondCharacter = remainingInput.tail.headOption
    lazy val remainingAfterNewLine = remainingInput.dropWhile(_ != '\n').tail

    // todo line number
    def singleCharacterResult(char: Char) =
      TokenType.SingleCharacter
        .fromString(char.toString)
        .map(tokenType => ValidToken(Token.SimpleToken(tokenType, 0)))
        .toRight(
          ScannerError.ParseError(
            0,
            "",
            "Unexpected character parsing one character token.",
            char.toString
          )
        )

    val nextToken: Either[ScannerError, ScannerResult] = firstCharacter match
      case None => Right(EOF)
      case Some(char) =>
        char match
          case '\n' => Right(NewLine)
          case ' '  => Right(Space)
          case _ if TwoCharacter.entrypoints.contains(char) || char == '/' =>
            secondCharacter match
              case None => singleCharacterResult(char)
              case Some(char2) =>
                char2 match
                  case '\n' => // the newline will be consumed on the next pass
                    singleCharacterResult(char)
                  case ' ' => singleCharacterResult(char)
                  case '/' => Right(Comment)
                  case _ =>
                    val lexeme = char.toString + char2.toString
                    TwoCharacter
                      .fromString(lexeme)
                      .map(tokenType =>
                        ValidToken(Token.SimpleToken(tokenType, 0))
                      )
                      .toRight(
                        ScannerError.ParseError(
                          0,
                          "",
                          "Unexpected character parsing two character token.",
                          lexeme
                        )
                      )
          case _ => singleCharacterResult(char)

    nextToken match
      case Right(EOF)   => results
      case Right(Space) => scanLoop(remainingInput.tail, results)
      case Right(NewLine) | Right(Comment) =>
        scanLoop(remainingAfterNewLine, results)
      case Right(ValidToken(token)) =>
        scanLoop(remainingInput.drop(token.length), results :+ Right(token))
      case Left(err) =>
        scanLoop(remainingInput.drop(err.lexeme.length), results :+ Left(err))
  }

  enum ScannerResult:
    case ValidToken(token: Token)
    case EOF, Space, Comment, NewLine
