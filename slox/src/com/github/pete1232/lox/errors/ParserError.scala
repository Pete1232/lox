package com.github.pete1232.lox.errors

import com.github.pete1232.lox.Token

import cats.implicits.*

enum ParserError(message: String, token: Token, lineNumber: Int)
    extends Throwable:

  case UnmatchedTokenError(expressionType: String, line: Int, token: Token)
      extends ParserError(
        s"Error trying to parse ${token.show} as $expressionType.",
        token,
        line,
      )
