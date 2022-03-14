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

  override def toString = s"${this.getClass.getSimpleName}: $message [$lexeme]"
