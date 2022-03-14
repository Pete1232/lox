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
        case SimpleToken(tokenType, line)         =>
          s"[$line] $tokenType"
        case LiteralString(lexeme, literal, line) =>
          s"[$line] $lexeme, $literal"
        case LiteralNumber(lexeme, literal, line) =>
          s"[$line] $lexeme, $literal"
    }

  final case class SimpleToken(
      tokenType: FixedTokenType,
      line: Int,
  ) extends Token:
    final val length: Int = tokenType.length

  final case class LiteralString(
      lexeme: String,
      literal: String,
      line: Int,
  ) extends Token:
    final val tokenType   = TokenType.Literal.StringLiteral
    final val length: Int = lexeme.length

  final case class LiteralNumber(
      lexeme: String,
      literal: Double,
      line: Int,
  ) extends Token:
    final val tokenType = TokenType.Literal.NumberLiteral

    final val length: Int = lexeme.length
