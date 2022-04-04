package com.github.pete1232.lox

import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.Show
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import cats.implicits.*

final case class Runner(scanner: Scanner, parser: Parser)(implicit
    console: Console[IO]
):

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
    val (scannerErrors, scannedTokens) =
      scanner.scan(source).partitionMap(identity)
    if scannerErrors.nonEmpty then
      scannerErrors
        .map { scanError =>
          import scanError.*
          IO.println(s"[line $lineNumber] Error: $message")
        }
        .sequence_
        .as(ExitCode(65))
    else
      val (parserErrors, parsedExpressions) =
        parser.parse(scannedTokens).partitionMap(identity)
      if parserErrors.nonEmpty then
        parserErrors
          .map { parseError =>
            import parseError.*
            IO.println(s"[line $lineNumber] Error: $message")
          }
          .sequence_
          .as(ExitCode.Error)
      else parsedExpressions.map(IO.println).sequence_.as(ExitCode.Success)

  object ErrorHandler:

    val repl: PartialFunction[Throwable, IO[ExitCode]] = {
      case _: EOFException => IO.println(":quit").as(ExitCode.Success)
    }

    val file: PartialFunction[Throwable, IO[ExitCode]] = {
      case nsfe: NoSuchFileException =>
        IO.println(s"File not found at path ${nsfe.getFile}").as(ExitCode.Error)
    }

end Runner
