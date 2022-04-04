package com.github.pete1232.lox.errors

import com.github.pete1232.lox.Token

import cats.Show
import cats.implicits.*

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
