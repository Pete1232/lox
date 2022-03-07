package com.github.pete1232.lox

import scala.io.Source
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Lox:
  @main def main(args: String*): Unit =
    if (args.length > 1)
      println("Usage: slox [script]")
      System.exit(64)
    else if (args.length == 1)
      runFile(args.head)
    else
      runPrompt()

  private def runFile(path: String): Unit =
    val bytes = Source.fromFile(path).map(_.byteValue).toArray
    run(new String(bytes, Charset.defaultCharset))

  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    @scala.annotation.tailrec
    def loop(): Unit = {
      print("> ")
      val line = Try(reader.readLine())

      line match
        case Success(l) =>
          if (l != null)
            println(l)
            run(l)
            loop()
          else println("Exit")
        case Failure(e) => println("Exception")
    }

    loop()
  }

  private def run(source: String): Unit = {
    // todo for now just printing the tokens
    for (token <- Scanner(source).tokens) println(token)
  }
