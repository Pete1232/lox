package com.github.pete1232

import com.github.pete1232.lox.utils.Showable

package object lox:
  type LoxValue = Double | String | Boolean | Null

  given Showable[LoxValue] with
    extension (lv: LoxValue)
      def show: String =
        lv match
          case d: Double  => d.show
          case s: String  => s.show
          case b: Boolean => b.show
          case _          => "null"
