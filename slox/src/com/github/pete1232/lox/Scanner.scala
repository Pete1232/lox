package com.github.pete1232.lox

final case class Scanner(source: String):
  final lazy val tokens: List[Token] = List(Token(source))
