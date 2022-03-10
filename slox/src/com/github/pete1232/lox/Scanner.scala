package com.github.pete1232.lox

trait Scanner:
  def scan(source: String): List[Token]

object DefaultScanner extends Scanner:
  def scan(source: String): List[Token] = List(Token(source))
