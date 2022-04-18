package com.github.pete1232.lox

import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.io.Logging
import com.github.pete1232.lox.models.{Expression, Token}

import cats.effect.IO
import cats.effect.kernel.Sync
import org.typelevel.log4cats.Logger

sealed trait Interpreter[F[_], T]:
  extension (t: T) def interpret: F[Either[InterpreterError, LoxValue]]

final class ExpressionInterpreter[F[_]: Sync]
    extends Interpreter[F, Expression]
    with Logging:
  extension (expr: Expression)
    def interpret: F[Either[InterpreterError, LoxValue]] =
      withLogger {
        import cats.implicits.*
        expr match
          case l: Expression.Literal =>
            Logger[F]
              .trace(s"Interpreting literal ${l.show}")
              .as(Right(l.value))
          case g: Expression.Group   =>
            Logger[F].trace(
              s"Interpreting group ${g.show}"
            ) *> g.expression.interpret
          case u: Expression.Unary   =>
            Logger[F].trace(s"Interpreting unary ${u.show}") *> {
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
                case Token.SingleCharacter.Bang  =>
                  u.right.interpret.map(_.flatMap {
                    _ match
                      case bool: Boolean => Right(!bool)
                      case v             =>
                        Left(
                          InterpreterError.UnaryCastError(
                            v,
                            Token.SingleCharacter.Bang,
                          )
                        )
                  })
            }
          case _                     => ??? // todo exhaustive match
      }

object Interpreter:
  given Interpreter[IO, Expression] = new ExpressionInterpreter[IO]
