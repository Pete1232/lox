package com.github.pete1232.lox.errors

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

  case LiteralNumberBadCharacter(line: Int, override val lexeme: String)
      extends ScannerError(
        "A non-digit character was found in a numeric literal.",
        lexeme,
        line,
      )

  case LiteralNumberTwoPoints(line: Int, override val lexeme: String)
      extends ScannerError(
        "Found two decimal points in the same numeric literal",
        lexeme,
        line,
      )

  case LiteralIdentifierBadCharacter(line: Int, override val lexeme: String)
      extends ScannerError(
        "A non-alpha-numeric character was found in an identifier.",
        lexeme,
        line,
      )

  case UnclosedComment(line: Int, override val lexeme: String)
      extends ScannerError(
        "A comment was left unclosed.",
        lexeme,
        line,
      )

  override def toString =
    s"${this.getClass.getSimpleName}: $message [$lexeme] [line: $lineNumber]"

end ScannerError