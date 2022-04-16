package com.github.pete1232.lox

import com.github.pete1232.lox.utils.LoggerBootstrap

import scala.io.Source
import scala.util.Try

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource

object Main extends IOApp:

  val parser = DefaultParser

  final def run(args: List[String]): IO[ExitCode] =
    LoggerBootstrap.create().flatMap { logger =>
      Runner(new DefaultScanner(using logger), parser, logger).run(args)
    }
