package com.github.pete1232.lox.errors

import com.github.pete1232.lox.models.Token
import com.github.pete1232.lox.utils.Showable

enum ParserError(val message: String, val lineNumber: Int) extends Throwable:

  case UnmatchedTokenError(expressionType: String, line: Int, token: Token)
      extends ParserError(
        s"Error trying to parse ${token.show} as $expressionType.",
        line,
      )

  case IncompleteExpression(expressionType: String)
      extends ParserError(
        "All tokens were consumed before the expression was completed.",
        0,
      )

  case UnclosedGroupError(line: Int)
      extends ParserError(
        s"Expect ')' to close an expression.",
        line,
      )

  case IncompleteConditionalError(line: Int)
      extends ParserError(
        "A ternary expression was not completed. Expected a ? b : c",
        line,
      )

  case BinaryExpressionNotOpened(line: Int)
      extends ParserError(
        s"A binary expression was missing a left-hand operand.",
        line,
      )

object ParserError:
  given Showable[ParserError] with
    extension (error: ParserError)
      def show: String =
        import error.*
        s"[line $lineNumber] Error in parser: $message"
