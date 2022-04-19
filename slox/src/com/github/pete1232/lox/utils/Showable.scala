package com.github.pete1232.lox.utils

import cats.Show

/** Scala 3 implementation of show, with an implicit coversion back to cats for
  * compatibility
  */
trait Showable[T]:
  extension (t: T) def show: String

object Showable:

  given Showable[String] with
    extension (s: String) def show: String = s

  given Showable[Double] with
    extension (d: Double)
      def show: String =
        if d.isValidInt then d.intValue.toString else d.toString

  given Showable[Boolean] with
    extension (b: Boolean) def show: String = b.toString

  given Showable[Null] with
    extension (n: Null) def show: String = "nil"

  given [T](using Showable[T]): Show[T] = Show.show(_.show)
