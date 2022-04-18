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
                    case Token.SingleCharacter.Comma         =>
                      ??? // todo comma, eval and discard left
                    case Token.TwoCharacter.EqualEqual       =>
                      EitherT.fromEither(Right(isEqual(left, right)))
                    case Token.TwoCharacter.BangEqual        =>
                      EitherT.fromEither(Right(!isEqual(left, right)))
                    case t @ Token.SingleCharacter.Greater   =>
                      comparisonExpression(left, right, t)(_ > _)
                    case t @ Token.TwoCharacter.GreaterEqual =>
                      comparisonExpression(left, right, t)(_ >= _)
                    case t @ Token.SingleCharacter.Less      =>
                      comparisonExpression(left, right, t)(_ < _)
                    case t @ Token.TwoCharacter.LessEqual    =>
                      comparisonExpression(left, right, t)(_ <= _)
                    case t @ Token.SingleCharacter.Minus     =>
                      arithmeticExpression(left, right, t)(_ - _)
                    case t @ Token.SingleCharacter.Slash     =>
                      arithmeticExpression(left, right, t)(_ / _)
                    case t @ Token.SingleCharacter.Star      =>
                      arithmeticExpression(left, right, t)(_ * _)
                    case t @ Token.SingleCharacter.Plus      =>
                      (left, right) match
                        case (d1: Double, d2: Double) =>
                          EitherT.fromEither(Right(d1 + d2))
                        case (s1: String, s2: String) =>
                          EitherT.fromEither(Right(s1 + s2))
                        case _                        =>
                          throw InterpreterError.BinaryCastError(left, right, t)
                }
              yield result
            }.value

          case t: Expression.Ternary => ??? // todo ternary implementation
        end match
      }
  end extension

  private def booleanValue(value: LoxValue) =
    value match
      case null       => false
      case b: Boolean => b
      case _          => true

  private def isEqual(left: LoxValue, right: LoxValue) =
    if left == null && right == null then true
    else if left == null then false
    else left.equals(right)

  private def arithmeticExpression(
      left: LoxValue,
      right: LoxValue,
      token: Token,
  )(eval: (Double, Double) => Double): EitherT[F, InterpreterError, LoxValue] =
    (left, right) match
      case (d1: Double, d2: Double) =>
        EitherT.fromEither(Right(eval(d1, d2)))
      case _                        =>
        throw InterpreterError.BinaryCastError(
          left,
          right,
          token,
        )

  private def comparisonExpression(
      left: LoxValue,
      right: LoxValue,
      token: Token,
  )(eval: (Double, Double) => Boolean): EitherT[F, InterpreterError, LoxValue] =
    (left, right) match
      case (d1: Double, d2: Double) =>
        EitherT.fromEither(Right(eval(d1, d2)))
      case _                        =>
        throw InterpreterError.BinaryCastError(
          left,
          right,
          token,
        )

end ExpressionInterpreter

object Interpreter:
  given Interpreter[IO, Expression] = new ExpressionInterpreter[IO]
