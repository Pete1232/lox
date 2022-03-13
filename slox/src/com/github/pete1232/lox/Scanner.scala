package com.github.pete1232.lox

import com.github.pete1232.lox.TokenType.TwoCharacter

trait Scanner:
  def scan(source: String): List[Either[ScannerError, Token]]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Either[ScannerError, Token]] =
    scanLoop(source, 0, Nil)

  private def scanLoop(
      remainingInput: String,
      currentLine: Int,
      results: List[Either[ScannerError, Token]]
  ): List[Either[ScannerError, Token]] = {
    import ScannerResult.*

    val firstCharacter = remainingInput.headOption
    lazy val secondCharacter = remainingInput.tail.headOption
    lazy val remainingAfterNewLine = remainingInput.dropWhile(_ != '\n').tail

    def singleCharacterResult(char: Char) =
      TokenType.SingleCharacter
        .fromString(char.toString)
        .map(tokenType => ValidToken(Token.SimpleToken(tokenType, currentLine)))
        .toRight(
          ScannerError.ParseError(
            currentLine,
            "",
            "Unexpected character parsing one character token.",
            char.toString
          )
        )

    val nextToken: Either[ScannerError, ScannerResult] = firstCharacter match
      case None => Right(EOF)
      case Some(char) =>
        char match
          case '\n'              => Right(NewLine)
          case ' ' | '\r' | '\t' => Right(Space)
          case _ if TwoCharacter.entrypoints.contains(char) || char == '/' =>
            secondCharacter match
              case None => singleCharacterResult(char)
              case Some(char2) =>
                char2 match
                  case '\n' => // the newline will be consumed on the next pass
                    singleCharacterResult(char)
                  case ' ' | '\r' | '\t' => singleCharacterResult(char)
                  case '/'               => Right(Comment)
                  case _ =>
                    val lexeme = char.toString + char2.toString
                    TwoCharacter
                      .fromString(lexeme)
                      .map(tokenType =>
                        ValidToken(Token.SimpleToken(tokenType, currentLine))
                      )
                      .toRight(
                        ScannerError.ParseError(
                          currentLine,
                          "",
                          "Unexpected character parsing two character token.",
                          lexeme
                        )
                      )
          case _ => singleCharacterResult(char)

    nextToken match
      case Right(EOF)   => results
      case Right(Space) => scanLoop(remainingInput.tail, currentLine, results)
      case Right(NewLine) | Right(Comment) =>
        scanLoop(remainingAfterNewLine, currentLine + 1, results)
      case Right(ValidToken(token)) =>
        // todo should enforce some kind of whitespace after a valid token
        scanLoop(
          remainingInput.drop(token.length),
          currentLine,
          results :+ Right(token)
        )
      case Left(err) =>
        scanLoop(
          remainingInput.drop(err.lexeme.length),
          currentLine,
          results :+ Left(err)
        )
  }

  enum ScannerResult:
    case ValidToken(token: Token)
    case EOF, Space, Comment, NewLine
