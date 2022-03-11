package com.github.pete1232.lox

import weaver.SimpleIOSuite
import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import java.nio.charset.Charset
import java.io.EOFException

object RunnerSuite extends SimpleIOSuite:

  object MockScanner extends Scanner:
    def scan(source: String): List[Either[ScannerError, Token]] = Nil

  val runner = Runner(MockScanner)

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
    def error[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit
    def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit
    def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit
    def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit
    def readLineWithCharset(charset: Charset): IO[String] = in

  def runnerWithFakeConsole(in: IO[String]) =
    Runner(MockScanner)(FakeConsole(in))

  test("repl should exit with a success on EOF") {
    for exitCode <- runnerWithFakeConsole(IO.raiseError(new EOFException()))
        .run(Nil)
    yield expect(exitCode.code == 0)
  }
