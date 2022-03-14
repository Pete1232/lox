package com.github.pete1232.lox

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource
import cats.implicits.*
import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}
import cats.effect.std.Console

final case class Runner(scanner: Scanner)(implicit console: Console[IO]):

  final def run(args: List[String]): IO[ExitCode] =
    args match
      case Nil       => runPrompt()
      case hd :: Nil => runFile(hd)
      case _         => IO.println("Usage: slox [script]").as(ExitCode(64))

  private def runFile(path: String): IO[ExitCode] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap(runScan)
      .recoverWith(ErrorHandler.file)

  private def runPrompt(): IO[ExitCode] =
    (for
      _      <- console.print("> ")
      l      <- console.readLine
      _      <- runScan(l)
      result <- runPrompt()
    yield result).recoverWith(ErrorHandler.repl)

  private def runScan(source: String): IO[ExitCode] =
    // todo for now just printing the tokens
    scanner
      .scan(source)
      .map(IO.println)
      .sequence
      .as(ExitCode.Success)
      .recoverWith(ErrorHandler.scanner)

  object ErrorHandler:

    val repl: PartialFunction[Throwable, IO[ExitCode]] = {
      case _: EOFException => IO.println(":quit").as(ExitCode.Success)
    }

    val file: PartialFunction[Throwable, IO[ExitCode]] = {
      case nsfe: NoSuchFileException =>
        IO.println(s"File not found at path ${nsfe.getFile}").as(ExitCode.Error)
    }

    val scanner: PartialFunction[Throwable, IO[ExitCode]] = {
      case scan: ScannerError =>
        import scan.*
        IO.println(s"[line $lineNumber] Error: $message").as(ExitCode(65))
    }
