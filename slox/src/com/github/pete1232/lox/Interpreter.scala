package com.github.pete1232.lox

trait Interpreter[T]:
  extension (t: T) def interpret: LoxValue

object Interpreter:

  given Interpreter[Expression.Literal] with
    extension (literal: Expression.Literal)
      def interpret: LoxValue = literal.value
