package com.github.pete1232.lox.utils

extension (c: Char)
  def isAlpha    = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  def isNum      = c >= '0' && c <= '9'
  def isAlphaNum = c.isAlpha || c.isNum
