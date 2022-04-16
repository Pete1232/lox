package com.github.pete1232.lox.utils

import scala.language.unsafeNulls

import cats.effect.IO
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Abstract away the cats logger implementation. Specifically restrict the use
  * of unsafe nulls. Also only needs to support IO.
  */
trait LoggerBootstrap:
  def create(): IO[SelfAwareStructuredLogger[IO]]

  def getUnsafeLogger(): SelfAwareStructuredLogger[IO]

object LoggerBootstrap:
  inline def create(): IO[SelfAwareStructuredLogger[IO]] =
    import scala.language.unsafeNulls
    Slf4jLogger.create[IO]

  inline def getUnsafeLogger(): SelfAwareStructuredLogger[IO] =
    import scala.language.unsafeNulls
    Slf4jLogger.getLogger[IO]
