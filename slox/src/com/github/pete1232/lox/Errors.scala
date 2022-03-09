package com.github.pete1232.lox

sealed trait ScannerError extends Throwable

object ScannerError:
  case object ParseError extends ScannerError
