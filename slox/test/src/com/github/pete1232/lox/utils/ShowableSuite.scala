package com.github.pete1232.lox.utils

import com.github.pete1232.lox.utils.Showable.given

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ShowableSuite extends SimpleIOSuite with Checkers:
  test("show a string as itself") {
    forall { (v: String) =>
      expect(v.show == v)
    }
  }
  test("show a double using Java toString") {
    forall(Gen.double.filter(!_.isValidInt)) { v =>
      expect(v.show == v.toString)
    }
  }
  test("show a valid integer using Java toString") {
    forall(Gen.posNum[Int]) { v =>
      expect(v.toDouble.show == v.toString)
    }
  }
  test("show a boolean using Java toString") {
    forall(Gen.oneOf(true, false)) { v =>
      expect(v.show == v.toString)
    }
  }
  pureTest("show null as nil") {
    expect(summon[Showable[Null]].show(null) == "nil")
  }
