package dev.adventofcode

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}

class Problem8Test extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "codeCount"

  it should "count \"\" correctly" in {
    val str = "\"\""
    Problem8.codeCount(str) shouldEqual 2
  }

  it should "count \"abc\" correctly" in {
    val str = "\"abc\""
    Problem8.codeCount(str) shouldEqual 5
  }

  it should "count \"aaa\\\"aaa\" correctly" in {
    val str = "\"aaa\\\"aaa\""
    Problem8.codeCount(str) shouldEqual 10
  }

  it should "count \"\\x27\" correctly" in {
    val str = "\"\\x27\""
    Problem8.codeCount(str) shouldEqual 6
  }


  behavior of "memoryCount"

  it should "count \"\" correctly" in {
    val str = "\"\""
    Problem8.memoryCount(str) shouldEqual 0
  }

  it should "count \"abc\" correctly" in {
    val str = "\"abc\""
    Problem8.memoryCount(str) shouldEqual 3
  }

  it should "count \"aaa\\\"aaa\" correctly" in {
    val str = "\"aaa\\\"aaa\""
    Problem8.memoryCount(str) shouldEqual 7
  }

  it should "count \"\\x27\" correctly" in {
    val str = "\"\\x27\""
    Problem8.memoryCount(str) shouldEqual 1
  }


  behavior of "encodedCount"

  it should "count \"\" correctly" in {
    val str = "\"\""
    Problem8.encodedCount(str) shouldEqual 6
  }

  it should "count \"abc\" correctly" in {
    val str = "\"abc\""
    Problem8.encodedCount(str) shouldEqual 9
  }

  it should "count \"aaa\\\"aaa\" correctly" in {
    val str = "\"aaa\\\"aaa\""
    Problem8.encodedCount(str) shouldEqual 16
  }

  it should "count \"\\x27\" correctly" in {
    val str = "\"\\x27\""
    Problem8.encodedCount(str) shouldEqual 11
  }
}
