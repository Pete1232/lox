package com.github.pete1232.lox

import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource

object Main extends IOApp:

  val scanner = DefaultScanner
  val parser  = DefaultParser
  val runner  = Runner(scanner, parser)

  final def run(args: List[String]): IO[ExitCode] =
    runner.run(args)
