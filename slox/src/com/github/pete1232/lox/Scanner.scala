package com.github.pete1232.lox

import com.github.pete1232.lox.TokenType.TwoCharacter

trait Scanner:
  def scan(source: String): List[Either[ScannerError, Token]]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Either[ScannerError, Token]] =
    scanLoop(source, 0, Nil)

  final val WhitespaceCharacters = List(' ', '\t', '\r', '\n')

  private def scanLoop(
      remainingInput: String,
      currentLine: Int,
      results: List[Either[ScannerError, Token]]
  ): List[Either[ScannerError, Token]] = {
    import ScannerResult.*

    val firstCharacter = remainingInput.headOption
    lazy val secondCharacter = remainingInput.tail.headOption
    lazy val thirdCharacter = remainingInput.tail.tail.headOption
    lazy val charactersToWhitespace =
      remainingInput.takeWhile(c => !WhitespaceCharacters.contains(c))
    lazy val remainingAfterNewLine = remainingInput.dropWhile(_ != '\n').tail

    def singleCharacterResult(char: Char) =
      TokenType.SingleCharacter
        .fromString(char.toString)
        .map(tokenType => ValidToken(Token.SimpleToken(tokenType, currentLine)))
        .toRight(
          ScannerError.InvalidFirstCharacter(
            currentLine,
            charactersToWhitespace
          )
        )

    def twoCharacterResult(lexeme: String) =
      TokenType.TwoCharacter
        .fromString(lexeme)
        .map(tokenType =>
          ValidToken(
            Token.SimpleToken(tokenType, currentLine)
          )
        )
        .toRight(
          ScannerError.InvalidSecondCharacter(
            currentLine,
            charactersToWhitespace
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
                  case c if WhitespaceCharacters.contains(c) =>
                    singleCharacterResult(char)
                  case '/' => Right(Comment)
                  case _ =>
                    val result = twoCharacterResult(
                      char.toString + char2.toString
                    )
                    thirdCharacter match
                      case Some(char3)
                          if !WhitespaceCharacters.contains(char3) =>
                        result.flatMap { _ =>
                          Left(
                            ScannerError.ValidTwoCharacterNoWhitespace(
                              currentLine,
                              charactersToWhitespace
                            )
                          )
                        }
                      case _ => result

          case _ =>
            val result = singleCharacterResult(char)
            secondCharacter match
              case Some(char2) if !WhitespaceCharacters.contains(char2) =>
                result.flatMap { _ =>
                  Left(
                    ScannerError.ValidOneCharacterNoWhitespace(
                      currentLine,
                      charactersToWhitespace
                    )
                  )
                }
              case _ => result

    nextToken match
      case Right(EOF)   => results
      case Right(Space) => scanLoop(remainingInput.tail, currentLine, results)
      case Right(NewLine) | Right(Comment) =>
        scanLoop(remainingAfterNewLine, currentLine + 1, results)
      case Right(ValidToken(token)) =>
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
