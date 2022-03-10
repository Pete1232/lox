package com.github.pete1232.lox

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Resource
import cats.implicits.*
import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

object Lox extends IOApp:
  final def run(args: List[String]): IO[ExitCode] =
    args match
      case Nil       => runPrompt()
      case hd :: Nil => runFile(hd)
      case _ => IO.println("Usage: slox [script]").map(_ => ExitCode(64))

  private def runFile(path: String): IO[ExitCode] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap(runScan)
      .recoverWith(ErrorHandler.file)

  private def runPrompt(): IO[ExitCode] =
    (for
      _ <- IO.print("> ")
      l <- IO.readLine
      _ <- runScan(l)
      result <- runPrompt()
    yield result).recoverWith(ErrorHandler.repl)

  private def runScan(source: String): IO[ExitCode] =
    // todo for now just printing the tokens
    Scanner(source).tokens
      .map(IO.println)
      .sequence
      .map(_ => ExitCode.Success)
      .recoverWith(ErrorHandler.scanner)

  object ErrorHandler:

    val repl: PartialFunction[Throwable, IO[ExitCode]] = {
      case _: EOFException => IO.println(":quit").map(_ => ExitCode.Success)
    }

    val file: PartialFunction[Throwable, IO[ExitCode]] = {
      case _: NoSuchFileException =>
        IO.println(":file not found").map(_ => ExitCode.Error)
    }

    val scanner: PartialFunction[Throwable, IO[ExitCode]] = {
      case scan: ScannerError =>
        scan match
          case ScannerError.ParseError(line, where, message) =>
            IO.println(s"[line $line] Error$where: $message")
              .map(_ => ExitCode(65))
    }
