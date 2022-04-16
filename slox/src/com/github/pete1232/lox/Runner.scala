package com.github.pete1232.lox

import com.github.pete1232.lox.io.SimpleConsole

import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.catsSyntaxApplicativeError
import cats.syntax.all.catsSyntaxNestedFoldable
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

final case class Runner(
    scanner: Scanner,
    parser: Parser,
    logger: SelfAwareStructuredLogger[IO],
)(using
    console: SimpleConsole[IO]
):

  final def run(args: List[String]): IO[ExitCode] =
    args match
      case Nil       => runPrompt()
      case hd :: Nil => runFile(hd)
      case _         => console.println("Usage: slox [script]").as(ExitCode(64))

  private def runFile(path: String): IO[ExitCode] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap { s =>
        if (s != null) then runScan(s) else IO.pure(ExitCode.Error)
      }
      .recoverWith(ErrorHandler.file)

  private def runPrompt(): IO[ExitCode] =
    (for
      _      <- console.print("> ")
      l      <- console.readLine
      _      <- logger.debug(s"Parsed line $l")
      _      <- runScan(l)
      result <- runPrompt()
    yield result).recoverWith(ErrorHandler.repl)

  private def runScan(source: String): IO[ExitCode] =
    val (scannerErrors, scannedTokens) =
      scanner.scan(source).partitionMap(identity)
    if scannerErrors.nonEmpty then
      scannerErrors
        .map(console.println)
        .sequence_
        .as(ExitCode(65))
    else
      val (parserErrors, parsedExpressions) =
        parser.parse(scannedTokens).partitionMap(identity)
      if parserErrors.nonEmpty then
        parserErrors
          .map(console.println)
          .sequence_
          .as(ExitCode.Error)
      else parsedExpressions.map(console.println).sequence_.as(ExitCode.Success)

  object ErrorHandler:

    val repl: PartialFunction[Throwable, IO[ExitCode]] = {
      case _: EOFException => console.println(":quit").as(ExitCode.Success)
    }

    val file: PartialFunction[Throwable, IO[ExitCode]] = {
      case nsfe: NoSuchFileException =>
        console
          .println(s"File not found at path ${nsfe.getFile}")
          .as(ExitCode.Error)
    }

end Runner
