package dev.adventofcode.problem5

import org.scalatest.{Matchers, FlatSpec}

class Problem5Test extends FlatSpec with Matchers {

  behavior of "pairLetters"

  it should "pair apple correctly" in {
    Problem5.pairLetters("apple") shouldEqual List(
      ('a', 'p'),
      ('p', 'p'),
      ('p', 'l'),
      ('l', 'e')
    )
  }


  behavior of "isNice"

  it should "treat ugknbfddgicrmopn as nice" in {
    Problem5.isNice("ugknbfddgicrmopn") shouldBe true
  }

  it should "treat aaa as nice" in {
    Problem5.isNice("aaa") shouldBe true
  }

  it should "treat jchzalrnumimnmhp as naughty" in {
    Problem5.isNice("jchzalrnumimnmhp") shouldBe false
  }

  it should "treat haegwjzuvuyypxyu as naughty" in {
    Problem5.isNice("haegwjzuvuyypxyu") shouldBe false
  }

  it should "treat dvszwmarrgswjxmb as naughty" in {
    Problem5.isNice("dvszwmarrgswjxmb") shouldBe false
  }


  behavior of "countNice"

  it should "count the right number of nice words" in {
    val count = Problem5.countNice(Iterator(
      "ugknbfddgicrmopn",
      "aaa",
      "jchzalrnumimnmhp",
      "haegwjzuvuyypxyu",
      "dvszwmarrgswjxmb"
    ))

    count shouldBe 2
  }
}
