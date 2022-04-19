package com.github.pete1232.lox

import com.github.pete1232.lox.io.SimpleConsole
import com.github.pete1232.lox.models.{Expression, TokenWithContext}

import java.io.EOFException

import cats.effect.IO
import cats.effect.std.Console
import weaver.SimpleIOSuite

object RunnerSuite extends SimpleIOSuite:

  object MockScanner extends Scanner:
    def scan(
        source: String
    ): IO[List[Either[errors.ScannerError, TokenWithContext]]] = IO.pure(Nil)

  object MockParser extends Parser:
    def parse(
        tokens: List[TokenWithContext]
    ): IO[List[Either[errors.ParserError, Expression]]] = IO.pure(Nil)

  val runner =
    Runner(DefaultScanner, DefaultParser)

  // /usr/include/sysexits.h
  test("error when invoked with more than one argument") {
    for exitCode <- runner.run(List("too", "many"))
    yield expect(exitCode.code == 64)
  }

  test("error when an input file is not found") {
    for exitCode <- runner.run(List("slox/test/resources/Missing.lox"))
    yield expect(exitCode.code == 66)
  }

  test("return success when passed an empty file") {
    for exitCode <- runner.run(List("slox/test/resources/Empty.lox"))
    yield expect(exitCode.code == 0)
  }

  test("return success when passed a valid file") {
    for exitCode <- runner.run(List("slox/test/resources/HelloWorld.lox"))
    yield expect(exitCode.code == 0)
  }

  test("error for scanner errors") {
    for exitCode <- runner.run(List("slox/test/resources/ScannerError.lox"))
    yield expect(exitCode.code == 65)
  }

  test("error for parser errors") {
    for exitCode <- runner.run(List("slox/test/resources/ParserError.lox"))
    yield expect(exitCode.code == 65)
  }

  test("error for interpreter errors") {
    for exitCode <- runner.run(List("slox/test/resources/InterpreterError.lox"))
    yield expect(exitCode.code == 70)
  }

  def runnerWithFakeConsole(in: IO[String]) =
    Runner(MockScanner, MockParser)(using
      SimpleConsole.fakeConsole(IO.unit, in)
    )

  test("repl should exit with a success on EOF") {
    for exitCode <- runnerWithFakeConsole(IO.raiseError(new EOFException()))
        .run(Nil)
    yield expect(exitCode.code == 0)
  }

  test("deal with any unhandled error") {
    for exitCode <- runnerWithFakeConsole(IO.raiseError(new Throwable()))
        .run(Nil)
    yield expect(exitCode.code == 1)
  }
end RunnerSuite
