package com.github.pete1232.lox

import com.github.pete1232.lox.TokenType.TwoCharacter
import com.github.pete1232.lox.Token.LiteralString
import com.github.pete1232.lox.Token.LiteralIdentifier
import com.github.pete1232.lox.utils.*

trait Scanner:
  def scan(source: String): List[Either[ScannerError, Token]]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Either[ScannerError, Token]] =
    scanLoop(source, 0, Nil)

  @scala.annotation.tailrec
  private def scanLoop(
      remainingInput: String,
      currentLine: Int,
      results: List[Either[ScannerError, Token]],
  ): List[Either[ScannerError, Token]] =
    import ScannerResult.*

    val firstCharacter              = remainingInput.headOption
    lazy val secondCharacter        = remainingInput.tail.headOption
    lazy val thirdCharacter         = remainingInput.tail.tail.headOption
    lazy val charactersToWhitespace =
      remainingInput.takeWhile(c => !c.isWhitespace)
    lazy val remainingAfterNewLine  = remainingInput.dropWhile(_ != '\n').tail

    def singleCharacterResult(char: Char) =
      TokenType.SingleCharacter
        .fromString(char.toString)
        .map(tokenType => ValidToken(Token.SimpleToken(tokenType, currentLine)))
        .toRight(
          ScannerError.InvalidFirstCharacter(
            currentLine,
            charactersToWhitespace,
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
            charactersToWhitespace,
          )
        )

    // returns the lexeme, meaning the string quotes are included
    @scala.annotation.tailrec
    def consumeString(
        remaining: String,
        result: String = "",
        isOpen: Boolean = false,
    ): Either[ScannerError, String] =
      remaining.headOption match
        case None                 =>
          Left(
            ScannerError.LiteralStringNotClosed(
              currentLine,
              result,
            )
          )
        case Some(c) if c == '\\' =>
          remaining.tail.headOption match
            case Some('"') =>
              consumeString(remaining.tail.tail, result + c + '"', isOpen)
            case _         => consumeString(remaining.tail, result + c, isOpen)
        case Some(c) if c == '\n' =>
          Left(
            ScannerError.LiteralStringNotClosed(
              currentLine,
              result,
            )
          )
        case Some(c) if c == '"'  =>
          if isOpen then Right(result + c)
          else consumeString(remaining.tail, result + c, isOpen = true)
        case Some(c) => consumeString(remaining.tail, result + c, isOpen)

    @scala.annotation.tailrec
    def consumeIdentifier(
        remaining: String,
        result: String = "",
    ): Either[ScannerError, String] =
      remaining.headOption match
        case None                      =>
          Right(result)
        case Some(c) if c.isWhitespace =>
          Right(result)
        case Some(c)                   =>
          if c.isAlphaNum || c == '_' then
            consumeIdentifier(remaining.tail, result + c)
          else
            Left(
              ScannerError.LiteralIdentifierBadCharacter(
                currentLine,
                charactersToWhitespace,
              )
            )

    @scala.annotation.tailrec
    def consumeDigits(
        remaining: String,
        result: String = "",
        hasDecimalPoint: Boolean = false,
    ): Either[ScannerError, String] =
      remaining.headOption match
        case None                      => Right(result)
        case Some(c) if c.isWhitespace => Right(result)
        case Some(c) if c == '.'       =>
          if (hasDecimalPoint) then
            Left(
              ScannerError.LiteralNumberTwoPoints(
                currentLine,
                charactersToWhitespace,
              )
            )
          else consumeDigits(remaining.tail, result + c, true)
        case Some(c)                   =>
          if c >= '0' && c <= '9' then
            consumeDigits(remaining.tail, result + c, hasDecimalPoint)
          else
            Left(
              ScannerError.LiteralNumberBadCharacter(
                currentLine,
                charactersToWhitespace,
              )
            )

    val nextToken: Either[ScannerError, ScannerResult] = firstCharacter match
      case None       => Right(EOF)
      case Some(char) =>
        char match
          case '\n'              => Right(NewLine)
          case ' ' | '\r' | '\t' => Right(Space)
          case '"'               =>
            consumeString(remainingInput).map(stringValue =>
              ValidToken(
                LiteralString(
                  stringValue,
                  StringContext.processEscapes(
                    stringValue.substring(1, stringValue.length - 1)
                  ),
                  currentLine,
                )
              )
            )
          case _ if char.isAlpha =>
            consumeIdentifier(remainingInput).map(stringValue =>
              ValidToken(
                TokenType.Keyword.fromString(stringValue) match
                  case None          =>
                    LiteralIdentifier(
                      stringValue,
                      currentLine,
                    )
                  case Some(keyword) =>
                    Token.SimpleToken(
                      keyword,
                      currentLine,
                    )
              )
            )
          case _ if TwoCharacter.entrypoints.contains(char) || char == '/' =>
            secondCharacter match
              case None        => singleCharacterResult(char)
              case Some(char2) =>
                char2 match
                  case c if c.isWhitespace =>
                    singleCharacterResult(char)
                  case '/'                 => Right(Comment)
                  case '*'                 =>
                    val fullComment  = remainingInput.tail.takeWhile(_ != '/')
                    val newLineCount = fullComment.count(_ == '\n')
                    // todo find the end properly
                    Right(
                      MultiLineComment(
                        length = fullComment.length + 2,
                        lines = newLineCount,
                      )
                    )
                  case _                   =>
                    val result = twoCharacterResult(
                      char.toString + char2.toString
                    )
                    thirdCharacter match
                      case Some(char3) if !char3.isWhitespace =>
                        result.flatMap { _ =>
                          Left(
                            ScannerError.ValidTwoCharacterNoWhitespace(
                              currentLine,
                              charactersToWhitespace,
                            )
                          )
                        }
                      case _                                  => result
          case _ if char.isDigit                                           =>
            consumeDigits(remainingInput).map(lexeme =>
              ScannerResult.ValidToken(
                Token.LiteralNumber(
                  lexeme,
                  lexeme.toDouble,
                  currentLine,
                )
              )
            )
          case _                                                           =>
            val result = singleCharacterResult(char)
            secondCharacter match
              case Some(char2) if !char2.isWhitespace =>
                result.flatMap { _ =>
                  Left(
                    ScannerError.ValidOneCharacterNoWhitespace(
                      currentLine,
                      charactersToWhitespace,
                    )
                  )
                }
              case _                                  => result

    nextToken match
      case Right(EOF)   => results
      case Right(Space) => scanLoop(remainingInput.tail, currentLine, results)
      case Right(NewLine) | Right(Comment)        =>
        scanLoop(remainingAfterNewLine, currentLine + 1, results)
      case Right(MultiLineComment(length, lines)) =>
        scanLoop(remainingInput.drop(length), currentLine + lines, results)
      case Right(ValidToken(token))               =>
        scanLoop(
          remainingInput.drop(token.length),
          currentLine,
          results :+ Right(token),
        )
      case Left(err)                              =>
        scanLoop(
          remainingInput.drop(err.lexeme.length),
          currentLine,
          results :+ Left(err),
        )
  end scanLoop

  enum ScannerResult:
    case ValidToken(token: Token)
    case MultiLineComment(length: Int, lines: Int)
    case EOF, Space, Comment, NewLine
end DefaultScanner
