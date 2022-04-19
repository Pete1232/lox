package com.github.pete1232.lox

import com.github.pete1232.lox.Interpreter.given
import com.github.pete1232.lox.errors.InterpreterError
import com.github.pete1232.lox.io.Logging
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
) extends Logging:

  final def run(args: List[String]): IO[ExitCode]                   =
    withLogger {
      {
        args match
          case Nil       => runPrompt
          case hd :: Nil => runFile(hd)
          case _ => console.println("Usage: slox [script]").as(ExitCode(64))
      }.recoverWith { case t =>
        Logger[IO]
          .error(t)(s"Unhandled error ${t.getMessage}")
          .as(ExitCode.Error)
      }
    }
  private def runFile(path: String)(using Logger[IO]): IO[ExitCode] =
    IO
      .blocking(Files.readString(Path.of(path)))
      .flatMap { s =>
        if (s != null) then runScan(s) else IO.pure(ExitCode(66))
      }
      .recoverWith { case nsfe: NoSuchFileException =>
        console
          .println(s"File not found at path ${nsfe.getFile}")
          .as(ExitCode(66))
      }

  private def runPrompt(using Logger[IO]): IO[ExitCode] =
    (for
      _      <- console.print("> ")
      l      <- console.readLine
      _      <- Logger[IO].debug(s"Parsed line $l")
      _      <- runScan(l)
      result <- runPrompt
    yield result).recoverWith { case _: EOFException =>
      console.println(":quit").as(ExitCode.Success)
    }

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
        parser.parse(scannedTokens).flatMap { scanResult =>
          val (parserErrors, parsedExpressions) =
            scanResult.partitionMap(identity)
          if parserErrors.nonEmpty then
            parserErrors
              .map(console.println)
              .sequence_
              .as(ExitCode(65))
          else
            import cats.syntax.all.toTraverseOps
            parsedExpressions
              .map(_.interpret)
              .sequence
              .flatMap(console.println)
              .as(ExitCode.Success)
              .recoverWith { case error: InterpreterError =>
                console.println(error).as(ExitCode(70))
              }
        }
    }

end Runner
