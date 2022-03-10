package com.github.pete1232.lox

sealed trait ScannerError extends Throwable

object ScannerError:
  case class ParseError(line: Int, where: String, message: String)
      extends ScannerError
