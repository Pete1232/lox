package com.github.pete1232.lox

import cats.effect.{IO, IOApp}
import cats.effect.ExitCode
import cats.effect.kernel.Resource
import cats.implicits.*
import scala.io.Source
import scala.util.*

import java.io.EOFException
import java.nio.file.Files
import java.nio.file.Path

object Lox extends IOApp:
  final def run(args: List[String]): IO[ExitCode] =
    args match
      case Nil       => runPrompt().map(_ => ExitCode.Success)
      case hd :: Nil => runFile(hd).map(_ => ExitCode.Success)
      case _ => IO.println("Usage: slox [script]").map(_ => ExitCode(64))

  private def runFile(path: String): IO[Unit] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap(runScan)

  private def runPrompt(): IO[Unit] =
    (for
      _ <- IO.print("> ")
      l <- IO.readLine
      _ <- runScan(l)
      _ <- runPrompt()
    yield (): Unit).recoverWith { case e: EOFException => IO.println(":quit") }

  private def runScan(source: String): IO[Unit] = {
    // todo for now just printing the tokens
    Scanner(source).tokens.map(IO.println).sequence.map(_ => (): Unit)
  }
