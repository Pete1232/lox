package com.github.pete1232.lox

import com.github.pete1232.lox.io.LoggerBootstrap

import scala.io.Source
import scala.util.Try

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource

object Main extends IOApp:

  final def run(args: List[String]): IO[ExitCode] =
    Runner(DefaultScanner, DefaultParser).run(args)
