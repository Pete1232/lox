package com.github.pete1232.lox

import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.io.Logging
import com.github.pete1232.lox.models.{Expression, Token}

import cats.effect.IO
import org.typelevel.log4cats.Logger

trait Interpreter[T]:
  extension (t: T) def interpret: IO[Either[InterpreterError, LoxValue]]

object Interpreter extends Logging:

  given Interpreter[Expression] with
    extension (expr: Expression)

      private def impl(using
          Logger[IO]
      ): IO[Either[InterpreterError, LoxValue]] = expr match
        case l: Expression.Literal =>
          Logger[IO].trace(s"Interpreting literal ${l.show}").as(Right(l.value))
        case g: Expression.Group   =>
          Logger[IO].trace(
            s"Interpreting group ${g.show}"
          ) *> g.expression.interpret
        case u: Expression.Unary   =>
          Logger[IO].trace(s"Interpreting unary ${u.show}") *> {
            u.operator match
              case Token.SingleCharacter.Minus =>
                u.right.interpret.map(_.flatMap { result =>
                  result match
                    case dbl: Double => Right(-dbl)
                    case v           =>
                      Left(
                        InterpreterError.UnaryCastError(
                          v,
                          Token.SingleCharacter.Minus,
                        )
                      )
                })
              case Token.SingleCharacter.Bang  => ??? // todo
          }
        case _                     => ??? // todo exhaustive match

      def interpret: IO[Either[InterpreterError, LoxValue]] =
        withLogger(impl)
