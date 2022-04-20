package com.github.pete1232.lox.errors

import com.github.pete1232.lox.LoxValue
import com.github.pete1232.lox.given
import com.github.pete1232.lox.models.Token
import com.github.pete1232.lox.utils.Showable

enum InterpreterError(val message: String, val lineNumber: Int)
    extends RuntimeException:

  case UnaryCastError(
      value: LoxValue,
      operatorToken: Token,
      line: Int,
  ) extends InterpreterError(
        s"The expression ${operatorToken.show} cannot preceed expression ${value.show}.",
        line,
      )

  case BinaryCastError(
      left: LoxValue,
      right: LoxValue,
      operatorToken: Token,
      line: Int,
  ) extends InterpreterError(
        s"The expression ${operatorToken.show} cannot operate on ${left.show} and ${right.show}.",
        line,
      )

  case DivideByZero(
      line: Int
  ) extends InterpreterError("Cannot divide by 0.", line)

  case TernaryCastError(
      left: LoxValue,
      line: Int,
  ) extends InterpreterError(
        s"Found ${left.show} at the start of a ternary expression, but expected a boolean.",
        line,
      )

object InterpreterError:

  given Showable[InterpreterError] with
    extension (error: InterpreterError)
      def show: String =
        import error.*
        s"[line $lineNumber] Error in interpreter: $message"
