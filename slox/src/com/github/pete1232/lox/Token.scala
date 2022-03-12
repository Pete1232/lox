package com.github.pete1232.lox

import cats.Show

sealed trait Token:
  def tokenType: TokenType
  def line: Int

object Token:
  implicit val showToken: Show[Token] =
    Show.show { t =>
      t match
        case SimpleToken(tokenType, line) =>
          s"[$line] $tokenType"
        case LiteralToken(tokenType, lexeme, literal, line) =>
          s"[$line] $tokenType, $lexeme, $literal"
    }

  final case class SimpleToken(
      tokenType: TokenType,
      line: Int
  ) extends Token

  final case class LiteralToken(
      tokenType: TokenType,
      lexeme: String,
      literal: Object,
      line: Int
  ) extends Token
