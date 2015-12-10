package dev.adventofcode

import org.scalatest.{Matchers, FlatSpec}

class Problem10Test extends FlatSpec with Matchers {

  behavior of "look and say"

  it should "transform 1 correctly" in {
    Problem10.lookAndSay("1") shouldEqual "11"
  }

  it should "transform 11 correctly" in {
    Problem10.lookAndSay("11") shouldEqual "21"
  }

  it should "transform 21 correctly" in {
    Problem10.lookAndSay("21") shouldEqual "1211"
  }

  it should "transform 1211 correctly" in {
    Problem10.lookAndSay("1211") shouldEqual "111221"
  }

  it should "transform 111221 correctly" in {
    Problem10.lookAndSay("111221") shouldEqual "312211"
  }

}
