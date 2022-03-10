package com.github.pete1232.lox

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck._

object ScannerSuite extends SimpleIOSuite with Checkers:

  test("return a Token containing the whole input") {
    forall(Gen.alphaStr) { s =>
      expect(DefaultScanner.scan(s) == List(Token(s)))
    }
  }
