package com.github.pete1232.lox.io

import scala.language.unsafeNulls

import cats.effect.kernel.Sync
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Make log4cats easier to work with, and retrict the use of unsafe nulls. */
trait Logging:

  import cats.syntax.all.toFlatMapOps
  import cats.syntax.all.toFunctorOps

  inline def withLogger[F[_]: Sync, T](f: Logger[F] ?=> F[T]): F[T] =
    import scala.language.unsafeNulls
    for
      given Logger[F] <- Slf4jLogger.create[F]
      result          <- f
    yield result
