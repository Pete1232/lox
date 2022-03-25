package com.github.pete1232.lox

import com.github.pete1232.lox.errors.ParserError

sealed trait Parser:
  def parse(
      tokens: List[TokenWithContext]
  ): List[Either[ParserError, Expression]]

object DefaultParser extends Parser:

  import ParserError.*
  def parse(
      tokens: List[TokenWithContext]
  ): List[Either[ParserError, Expression]] =
    tokens.map(primary)

  private def primary(
      token: TokenWithContext
  ): Either[ParserError, Expression] =
    token.token match
      case Token.Keyword.False       => Right(Expression.Literal(false))
      case Token.Keyword.True        => Right(Expression.Literal(true))
      case Token.Keyword.Nil         => Right(Expression.Literal(null))
      case Token.LiteralNumber(_, n) => Right(Expression.Literal(n))
      case Token.LiteralString(_, s) => Right(Expression.Literal(s))
      case _                         =>
        Left(
          UnmatchedTokenError("primary", token.context.lineCount, token.token)
        )
