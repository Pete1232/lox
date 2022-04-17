package com.github.pete1232.lox

import com.github.pete1232.lox.io.LoggerBootstrap
import com.github.pete1232.lox.io.SimpleConsole

import scala.io.Source

import java.io.EOFException
import java.nio.file.{Files, NoSuchFileException, Path}

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.catsSyntaxApplicativeError
import cats.syntax.all.catsSyntaxNestedFoldable
import org.typelevel.log4cats.Logger

final case class Runner(
    scanner: Scanner,
    parser: Parser,
)(using
    console: SimpleConsole[IO]
):

  final def run(args: List[String]): IO[ExitCode] =
    for
      logger <- LoggerBootstrap.create()
      result <- {
        args match
          case Nil       => runPrompt(using logger)
          case hd :: Nil => runFile(hd)(using logger)
          case _ => console.println("Usage: slox [script]").as(ExitCode(64))
      }
    yield result

  private def runFile(path: String)(using Logger[IO]): IO[ExitCode] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap { s =>
        if (s != null) then runScan(s) else IO.pure(ExitCode.Error)
      }
      .recoverWith(ErrorHandler.file)

  private def runPrompt(using Logger[IO]): IO[ExitCode] =
    (for
      _      <- console.print("> ")
      l      <- console.readLine
      _      <- Logger[IO].debug(s"Parsed line $l")
      _      <- runScan(l)
      result <- runPrompt
    yield result).recoverWith(ErrorHandler.repl)

  private def runScan(source: String): IO[ExitCode] =
    scanner.scan(source).flatMap { scanResult =>
      val (scannerErrors, scannedTokens) =
        scanResult.partitionMap(identity)
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
        else
          parsedExpressions.map(console.println).sequence_.as(ExitCode.Success)
    }

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
