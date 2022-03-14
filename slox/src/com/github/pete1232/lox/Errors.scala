package com.github.pete1232.lox

import cats.Show

enum ScannerError(message: String, val lexeme: String) extends Throwable:
  case ParseError(
      line: Int,
      where: String,
      message: String,
      override val lexeme: String
  ) extends ScannerError(message, lexeme)

  override def toString = s"${this.getClass.getSimpleName}: $message [$lexeme]"
