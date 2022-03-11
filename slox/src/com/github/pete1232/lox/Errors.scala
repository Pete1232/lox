package com.github.pete1232.lox

enum ScannerError(message: String) extends Throwable:
  case ParseError(line: Int, where: String, message: String)
      extends ScannerError(message)
