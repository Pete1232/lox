package com.github.pete1232.lox

import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.models.{Expression, Token}

trait Interpreter[T]:
  extension (t: T) def interpret: Either[InterpreterError, LoxValue]

object Interpreter:

  given Interpreter[Expression] with
    extension (expr: Expression)
      def interpret: Either[InterpreterError, LoxValue] = expr match
        case l: Expression.Literal => Right(l.value)
        case g: Expression.Group   => g.expression.interpret
        case u: Expression.Unary   =>
          u.operator match
            case Token.SingleCharacter.Minus =>
              u.right.interpret.flatMap { result =>
                result match
                  case dbl: Double => Right(-dbl)
                  case v           =>
                    Left(
                      InterpreterError.UnaryCastError(
                        v,
                        Token.SingleCharacter.Minus,
                      )
                    )
              }
            case Token.SingleCharacter.Bang  => ??? // todo
        case _                     => ??? // todo exhaustive match
