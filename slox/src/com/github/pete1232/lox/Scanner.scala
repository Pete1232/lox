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
      import TokenType.*
      import cats.implicits._
      val tokenType = c match
        case '(' => SingleCharacter.LeftParen.some
        case ')' => SingleCharacter.RightParen.some
        case '{' => SingleCharacter.LeftBrace.some
        case '}' => SingleCharacter.RightBrace.some
        case ',' => SingleCharacter.Comma.some
        case '.' => SingleCharacter.Dot.some
        case '-' => SingleCharacter.Minus.some
        case '+' => SingleCharacter.Plus.some
        case ';' => SingleCharacter.Semicolon.some
        case '*' => SingleCharacter.Star.some
        case _   => None

      tokenType match
        case None =>
          Left(ScannerError.ParseError(0, "", "Unexpected character."))
        case Some(t) => Token.SimpleToken(t, 0).asRight
    }

    nextToken match
      case None => results // todo this should be an error?
      case Some(lex) if remainingInput.isEmpty => results :+ lex
      case Some(lex) => scanLoop(remainingInput.tail, results :+ lex)
  }
