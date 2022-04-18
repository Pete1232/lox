package com.github.pete1232.lox

import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.io.Logging
import com.github.pete1232.lox.models.{Expression, Token}

import cats.Functor
import cats.data.EitherT
import cats.effect.IO
import cats.effect.kernel.Sync
import org.typelevel.log4cats.Logger

sealed trait Interpreter[F[_], T]:
  extension (t: T) def interpret: F[Either[InterpreterError, LoxValue]]

final class ExpressionInterpreter[F[_]: Sync: Functor]
    extends Interpreter[F, Expression]
    with Logging:
  extension (expr: Expression)
    def interpret: F[Either[InterpreterError, LoxValue]] =
      withLogger {
        // import cats.implicits.*
        import cats.syntax.all.toFunctorOps
        import cats.syntax.all.catsSyntaxApply
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
                        throw InterpreterError.UnaryCastError(
                          v,
                          Token.SingleCharacter.Minus,
                        )
                  })
                case Token.SingleCharacter.Bang  =>
                  u.right.interpret.map(_.map(v => !booleanValue(v)))
            }
          case b: Expression.Binary  =>
            {
              for
                left   <- EitherT(b.left.interpret)
                right  <- EitherT(b.right.interpret)
                result <- {
                  b.operator match
                    case t @ Token.SingleCharacter.Minus =>
                      arithmeticExpression(left, right, t)(_ - _)
                    case t @ Token.SingleCharacter.Slash =>
                      arithmeticExpression(left, right, t)(_ / _)
                    case t @ Token.SingleCharacter.Star  =>
                      arithmeticExpression(left, right, t)(_ * _)
                    case _                               => ??? // todo
                }
              yield result
            }.value

          case t: Expression.Ternary => ???
      }
  end extension

  private def booleanValue(value: LoxValue) =
    value match
      case null       => false
      case b: Boolean => b
      case _          => true

  private def arithmeticExpression(
      left: LoxValue,
      right: LoxValue,
      token: Token,
  )(eval: (Double, Double) => Double): EitherT[F, InterpreterError, Double] =
    (left, right) match
      case (d1: Double, d2: Double) =>
        EitherT.fromEither(Right(eval(d1, d2)))
      case (v, _)                   =>
        throw InterpreterError.BinaryCastError(
          left,
          right,
          token,
        )

end ExpressionInterpreter

object Interpreter:
  given Interpreter[IO, Expression] = new ExpressionInterpreter[IO]
