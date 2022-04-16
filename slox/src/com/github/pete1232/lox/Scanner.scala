package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ScannerError
import com.github.pete1232.lox.utils.*

import cats.effect.IO

trait Scanner:
  def scan(source: String): IO[List[Either[ScannerError, TokenWithContext]]]

final class DefaultScanner(using logger: Logs) extends Scanner:
  def scan(source: String): IO[List[Either[ScannerError, TokenWithContext]]] =
    scanLoop(source, 0, Nil)

  private def scanLoop(
      remainingInput: String,
      currentLine: Int,
      results: List[Either[ScannerError, TokenWithContext]],
  ): IO[List[Either[ScannerError, TokenWithContext]]] =
    import ScannerResult.*

    val firstCharacter              = remainingInput.headOption
    lazy val secondCharacter        = remainingInput.tail.headOption
    lazy val thirdCharacter         = remainingInput.tail.tail.headOption
    lazy val charactersToWhitespace =
      remainingInput.takeWhile(c => !c.isWhitespace)
    lazy val remainingAfterNewLine  = remainingInput.dropWhile(_ != '\n').tail

    def singleCharacterResult(
        char: Char
    ): IO[Either[ScannerError, ScannerResult]] =
      logger.debug(s"Scanning $char as a single character token") *>
        IO.pure(
          Token.SingleCharacter
            .fromString(char.toString)
            .map(token => ValidToken(token, currentLine))
            .toRight(
              ScannerError.InvalidFirstCharacter(
                currentLine,
                charactersToWhitespace,
              )
            )
        )

    def twoCharacterResult(lexeme: String) =
      logger.debug(s"Scanning $lexeme as a two character token") *>
        IO.pure(
          Token.TwoCharacter
            .fromString(lexeme)
            .map(token =>
              ValidToken(
                token,
                currentLine,
              )
            )
            .toRight(
              ScannerError.InvalidSecondCharacter(
                currentLine,
                charactersToWhitespace,
              )
            )
        )

    // returns the lexeme, meaning the string quotes are included
    @scala.annotation.tailrec
    def consumeString(
        remaining: String,
        result: String = "",
        isOpen: Boolean = false,
    ): IO[Either[ScannerError, String]] =
      remaining.headOption match
        case None                 =>
          IO.pure(
            Left(
              ScannerError.LiteralStringNotClosed(
                currentLine,
                result,
              )
            )
          )
        case Some(c) if c == '\\' =>
          remaining.tail.headOption match
            case Some('"') =>
              consumeString(remaining.tail.tail, result + c + '"', isOpen)
            case _         => consumeString(remaining.tail, result + c, isOpen)
        case Some(c) if c == '\n' =>
          IO.pure(
            Left(
              ScannerError.LiteralStringNotClosed(
                currentLine,
                result,
              )
            )
          )
        case Some(c) if c == '"'  =>
          if isOpen then IO.pure(Right(result + c))
          else consumeString(remaining.tail, result + c, isOpen = true)
        case Some(c) => consumeString(remaining.tail, result + c, isOpen)

    def consumeIdentifier(
        remaining: String,
        result: String = "",
    ): IO[Either[ScannerError, String]] =
      logger.debug("Consuming an identifier") *> {
        remaining.headOption match
          case None                      =>
            IO.pure(Right(result))
          case Some(c) if c.isWhitespace =>
            IO.pure(Right(result))
          case Some(c)                   =>
            if c.isAlphaNum || c == '_' then
              consumeIdentifier(remaining.tail, result + c)
            else
              IO.pure(
                Left(
                  ScannerError.LiteralIdentifierBadCharacter(
                    currentLine,
                    charactersToWhitespace,
                  )
                )
              )
      }

    def consumeDigits(
        remaining: String,
        result: String = "",
        hasDecimalPoint: Boolean = false,
    ): IO[Either[ScannerError, String]] =
      logger.debug("Consuming an identifier") *> {
        remaining.headOption match
          case None                      => IO.pure(Right(result))
          case Some(c) if c.isWhitespace => IO.pure(Right(result))
          case Some(c) if c == '.'       =>
            if (hasDecimalPoint) then
              IO.pure(
                Left(
                  ScannerError.LiteralNumberTwoPoints(
                    currentLine,
                    charactersToWhitespace,
                  )
                )
              )
            else consumeDigits(remaining.tail, result + c, true)
          case Some(c)                   =>
            if c >= '0' && c <= '9' then
              consumeDigits(remaining.tail, result + c, hasDecimalPoint)
            else if c.isAlpha then
              IO.pure(
                Left(
                  ScannerError.LiteralNumberBadCharacter(
                    currentLine,
                    charactersToWhitespace,
                  )
                )
              )
            else IO.pure(Right(result))
      }

    def consumeComment(
        remaining: String,
        result: String = "",
        canClose: Boolean = false,
    ): IO[Either[ScannerError, String]] =
      remaining.headOption match
        case None                =>
          IO.pure(Left(ScannerError.UnclosedComment(currentLine, result)))
        case Some(c) if c == '/' =>
          if canClose then IO.pure(Right(result + c))
          else consumeComment(remaining.tail, result + c, canClose = false)
        case Some(c) if c == '*' =>
          consumeComment(remaining.tail, result + c, canClose = true)
        case Some(c)             =>
          consumeComment(remaining.tail, result + c, canClose = false)

    val nextToken: IO[Either[ScannerError, ScannerResult]] =
      firstCharacter match
        case None       => IO.pure(Right(EOF))
        case Some(char) =>
          char match
            case '\n'              => IO.pure(Right(NewLine))
            case ' ' | '\r' | '\t' => IO.pure(Right(Space))
            case '"'               =>
              consumeString(remainingInput).map(stringResult =>
                stringResult.flatMap { stringValue =>
                  val unescapedString =
                    stringValue.substring(1, stringValue.length - 1)
                  if (unescapedString != null) then
                    Right(
                      ValidToken(
                        Token.LiteralString(
                          stringValue,
                          StringContext.processEscapes(unescapedString),
                        ),
                        currentLine,
                      )
                    )
                  else
                    Left(ScannerError.LiteralStringNotClosed(currentLine, ""))
                }
              )
            case _ if char.isAlpha =>
              consumeIdentifier(remainingInput).map { identifierResult =>
                identifierResult.map { stringValue =>
                  val token = Token.Keyword.fromString(stringValue) match
                    case None          =>
                      Token.LiteralIdentifier(
                        stringValue,
                        stringValue,
                      )
                    case Some(keyword) =>
                      keyword
                  ValidToken(token, currentLine)
                }
              }
            case _
                if Token.TwoCharacter.entrypoints.contains(
                  char
                ) || char == '/' =>
              secondCharacter match
                case None        => singleCharacterResult(char)
                case Some(char2) =>
                  char2 match
                    case c if c.isWhitespace =>
                      singleCharacterResult(char)
                    case '/' if char == '/'  => IO.pure(Right(Comment))
                    case '*' if char == '/'  =>
                      consumeComment(remainingInput).map(commentResult =>
                        commentResult.map { stringValue =>
                          MultiLineComment(
                            length = stringValue.length,
                            lines = stringValue.count(_ == '\n'),
                          )
                        }
                      )
                    case _                   =>
                      twoCharacterResult(char.toString + char2.toString)
                        .flatMap { result =>
                          result match
                            case Right(result) => IO.pure(Right(result))
                            case Left(error)   => singleCharacterResult(char)
                        }
            case _ if char.isDigit =>
              consumeDigits(remainingInput).map(digitResult =>
                digitResult.map { lexeme =>
                  ValidToken(
                    Token.LiteralNumber(
                      lexeme,
                      lexeme.toDouble,
                    ),
                    currentLine,
                  )
                }
              )
            case _                 => singleCharacterResult(char)

    nextToken.flatMap { tokenResult =>
      tokenResult match
        case Right(EOF)   => IO.pure(results)
        case Right(Space) => scanLoop(remainingInput.tail, currentLine, results)
        case Right(NewLine) | Right(Comment)        =>
          scanLoop(remainingAfterNewLine, currentLine + 1, results)
        case Right(MultiLineComment(length, lines)) =>
          scanLoop(remainingInput.drop(length), currentLine + lines, results)
        case Right(ValidToken(token, line))         =>
          scanLoop(
            remainingInput.drop(token.length),
            currentLine,
            results :+ Right(TokenWithContext(token, TokenContext(line))),
          )
        case Left(err)                              =>
          scanLoop(
            remainingInput.drop(err.lexeme.length),
            currentLine,
            results :+ Left(err),
          )
    }
  end scanLoop

  enum ScannerResult:
    case ValidToken(token: Token, line: Int)
    case MultiLineComment(length: Int, lines: Int)
    case EOF, Space, Comment, NewLine
end DefaultScanner
