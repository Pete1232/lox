package com.github.pete1232.lox

import java.io.EOFException
import java.nio.charset.Charset

import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import weaver.SimpleIOSuite

object RunnerSuite extends SimpleIOSuite:

  object MockScanner extends Scanner:
    def scan(
        source: String
    ): List[Either[errors.ScannerError, TokenWithContext]] = Nil

  object MockParser extends Parser:
    def parse(
        tokens: List[TokenWithContext]
    ): List[Either[errors.ParserError, Expression]] = Nil

  val runner = Runner(MockScanner, MockParser)

  test("error when the file is not found") {
    for exitCode <- runner.run(List("slox/test/resources/Missing.lox"))
    yield expect(exitCode.code == 1)
  }

  test("return success when passed an empty file") {
    for exitCode <- runner.run(List("slox/test/resources/Empty.lox"))
    yield expect(exitCode.code == 0)
  }

  test("return success when passed a valid file") {
    for exitCode <- runner.run(List("slox/test/resources/HelloWorld.lox"))
    yield expect(exitCode.code == 0)
  }

  case class FakeConsole(in: IO[String]) extends Console[IO]:
    def error[A](a: A)(implicit S: Show[A]): IO[Unit]     = IO.unit
    def errorln[A](a: A)(implicit S: Show[A]): IO[Unit]   = IO.unit
    def print[A](a: A)(implicit S: Show[A]): IO[Unit]     = IO.unit
    def println[A](a: A)(implicit S: Show[A]): IO[Unit]   = IO.unit
    def readLineWithCharset(charset: Charset): IO[String] = in

  def runnerWithFakeConsole(in: IO[String]) =
    Runner(MockScanner, MockParser)(using FakeConsole(in))

  test("repl should exit with a success on EOF") {
    for exitCode <- runnerWithFakeConsole(IO.raiseError(new EOFException()))
        .run(Nil)
    yield expect(exitCode.code == 0)
  }
