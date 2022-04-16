package com.github.pete1232

import com.github.pete1232.lox.utils.Showable
import com.github.pete1232.lox.utils.Showable.given

import cats.effect.IO

package object lox:
  type LoxValue = Double | String | Boolean | Null

  given Showable[LoxValue] with
    extension (lv: LoxValue)
      def show: String =
        lv match
          case d: Double  => summon[Showable[Double]].show(d)
          case s: String  => summon[Showable[String]].show(s)
          case b: Boolean => summon[Showable[Boolean]].show(b)
          case null       => summon[Showable[Null]].show(null)
