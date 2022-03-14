package com.github.pete1232.lox

import cats.Show

enum ScannerError(val message: String, val lexeme: String, val lineNumber: Int)
    extends Throwable:
  case ValidOneCharacterNoWhitespace(line: Int, override val lexeme: String)
      extends ScannerError(
        "No whitespace after single character token.",
        lexeme,
        line,
      )
  case ValidTwoCharacterNoWhitespace(line: Int, override val lexeme: String)
      extends ScannerError(
        "No whitespace after two character token.",
        lexeme,
        line,
      )
  case InvalidFirstCharacter(line: Int, override val lexeme: String)
      extends ScannerError(
        "Unexpected character parsing one character token.",
        lexeme,
        line,
      )
  case InvalidSecondCharacter(line: Int, override val lexeme: String)
      extends ScannerError(
        "Unexpected character parsing two character token.",
        lexeme,
        line,
      )

  case LiteralStringNotClosed(line: Int, override val lexeme: String)
      extends ScannerError(
        "A string literal was opened but not closed on the same line.",
        lexeme,
        line,
      )

  case LiteralStringBadEscape(line: Int, override val lexeme: String)
      extends ScannerError(
        "An invalid excape character was included in a string.",
        lexeme,
        line,
      )

  override def toString =
    s"${this.getClass.getSimpleName}: $message [$lexeme] [line: $lineNumber]"
