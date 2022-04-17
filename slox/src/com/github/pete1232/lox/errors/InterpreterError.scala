package com.github.pete1232.lox.errors

import com.github.pete1232.lox.LoxValue
import com.github.pete1232.lox.models.Token
import com.github.pete1232.lox.utils.Showable

// todo interpreter errors need more context for line numbers
enum InterpreterError(val message: String, val lineNumber: Int)
    extends Throwable:

  case UnaryCastError(
      value: LoxValue,
      operatorToken: Token,
  ) extends InterpreterError(
        s"The value $value is not valid after a unary operator ${operatorToken.lexeme}",
        0,
      )

object InterpreterError:

  given Showable[InterpreterError] with
    extension (error: InterpreterError)
      def show: String =
        import error.*
        s"[line $lineNumber] Error in interpreter: $message"
