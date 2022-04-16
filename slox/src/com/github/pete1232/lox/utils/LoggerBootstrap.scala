package com.github.pete1232.lox.utils

import scala.language.unsafeNulls

import cats.effect.IO
import cats.effect.kernel.Sync
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Abstract away the cats logger implementation. Specifically restrict the use
  * of unsafe nulls. Also only needs to support IO.
  */
trait LoggerBootstrap:
  def create(): IO[SelfAwareStructuredLogger[IO]]

  def getUnsafeLogger(): SelfAwareStructuredLogger[IO]

object LoggerBootstrap:
  def create(): IO[SelfAwareStructuredLogger[IO]] =
    Slf4jLogger.create[IO]

  def getUnsafeLogger(): SelfAwareStructuredLogger[IO] =
    Slf4jLogger.getLogger[IO]
