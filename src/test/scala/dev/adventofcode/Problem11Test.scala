package dev.adventofcode

import org.scalatest.{Matchers, FlatSpec}

class Problem11Test extends FlatSpec with Matchers {

  behavior of "rules"

  it should "include one increasing straight of length >= 3" in {
    Problem11.hasStraight("abc") shouldEqual true
    Problem11.hasStraight("abd") shouldEqual false
    Problem11.hasStraight("cantalopquail") shouldEqual true
  }

  it should "not contain the letters i, o, or l" in {
    Problem11.noIOL("i") shouldEqual false
    Problem11.noIOL("team") shouldEqual true // There's no i in team
    Problem11.noIOL("abco") shouldEqual false
    Problem11.noIOL("mall") shouldEqual false
  }

  it should "have at least two different, non-overlaping pairs of letters" in {
    Problem11.hasPairs("aabaa") shouldEqual false
    Problem11.hasPairs("aabbb") shouldEqual true
    Problem11.hasPairs("aardvarrk") shouldEqual true
  }

  it should "increment strings" in {
    Problem11.increment("xx") shouldEqual "xy"
    Problem11.increment("xz") shouldEqual "ya"
    Problem11.increment("azz") shouldEqual "baa"
  }
}
