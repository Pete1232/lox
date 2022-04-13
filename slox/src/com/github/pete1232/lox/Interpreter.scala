package com.github.pete1232.lox

trait Interpreter[T]:
  extension (t: T) def interpret: LoxValue

object Interpreter:

  given Interpreter[Expression] with
    extension (expr: Expression)
      def interpret: LoxValue = expr match
        case l: Expression.Literal => l.value
        case g: Expression.Group   => g.expression.interpret
        case _                     => ???
