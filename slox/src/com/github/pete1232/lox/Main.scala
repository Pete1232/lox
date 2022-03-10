package com.github.pete1232.lox

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource
import cats.implicits.*
import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

object Main extends IOApp:

  val scanner = DefaultScanner
  val runner = Runner(scanner)

  final def run(args: List[String]): IO[ExitCode] =
    runner.run(args)
