package com.github.pete1232.lox.io

import cats.Show
import cats.effect.IO
import cats.effect.std.Console

trait SimpleConsole[F[_]]:
  def print[A](a: A)(using S: Show[A]): F[Unit]
  def readLine: F[String]

object SimpleConsole:
  given ioConsole(using cats: Console[IO]): SimpleConsole[IO] =
    new SimpleConsole:
      def print[A](a: A)(using s: Show[A]): IO[Unit] = cats.print(a)(s)
      def readLine: IO[String]                       = cats.readLine

  def fakeConsole(printResult: IO[Unit], readResult: IO[String]) =
    new SimpleConsole[IO]:
      def print[A](a: A)(using S: Show[A]): IO[Unit] = printResult
      def readLine: IO[String]                       = readResult
