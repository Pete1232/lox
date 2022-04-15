package com.github.pete1232.lox.io

import com.github.pete1232.lox.utils.Showable
import com.github.pete1232.lox.utils.Showable.given

import cats.effect.IO
import cats.effect.std.Console

trait SimpleConsole[F[_]]:
  def print[A](a: A)(using S: Showable[A]): F[Unit]
  def readLine: F[String]

  def println[A](a: A)(using S: Showable[A]): F[Unit] = print(
    a.show + Option(System.lineSeparator).getOrElse("\n")
  )

object SimpleConsole:
  given ioConsole(using cats: Console[IO]): SimpleConsole[IO] =
    new SimpleConsole:
      def print[A](a: A)(using s: Showable[A]): IO[Unit] =
        cats.print(a.show)
      def readLine: IO[String]                           = cats.readLine

  def fakeConsole(printResult: IO[Unit], readResult: IO[String]) =
    new SimpleConsole[IO]:
      def print[A](a: A)(using S: Showable[A]): IO[Unit] = printResult
      def readLine: IO[String]                           = readResult
