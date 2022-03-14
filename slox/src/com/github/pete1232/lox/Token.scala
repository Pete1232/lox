package com.github.pete1232.lox

import cats.Show

sealed trait Token:
  def tokenType: TokenType
  def line: Int
  def length: Int

object Token:
  implicit val showToken: Show[Token] =
    Show.show { t =>
      t match
        case SimpleToken(tokenType, line)                   =>
          s"[$line] $tokenType"
        case LiteralToken(tokenType, lexeme, literal, line) =>
          s"[$line] $tokenType, $lexeme, $literal"
    }

  final case class SimpleToken(
      tokenType: FixedTokenType,
      line: Int,
  ) extends Token:
    final val length: Int = tokenType.length

  final case class LiteralToken(
      tokenType: TokenType.Literal,
      lexeme: String,
      literal: Object,
      line: Int,
  ) extends Token:
    final val length: Int = lexeme.length
