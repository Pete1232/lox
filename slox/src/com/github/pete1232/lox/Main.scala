package com.github.pete1232.lox

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp:

  final def run(args: List[String]): IO[ExitCode] =
    Runner(DefaultScanner, DefaultParser).run(args)
