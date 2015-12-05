package dev.adventofcode.problem5

import dev.adventofcode.problem5.Problem5.Part1Rules
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


  behavior of "Part1Rules.isNice"

  it should "treat ugknbfddgicrmopn as nice" in {
    Problem5.Part1Rules.isNice("ugknbfddgicrmopn") shouldBe true
  }

  it should "treat aaa as nice" in {
    Problem5.Part1Rules.isNice("aaa") shouldBe true
  }

  it should "treat jchzalrnumimnmhp as naughty" in {
    Problem5.Part1Rules.isNice("jchzalrnumimnmhp") shouldBe false
  }

  it should "treat haegwjzuvuyypxyu as naughty" in {
    Problem5.Part1Rules.isNice("haegwjzuvuyypxyu") shouldBe false
  }

  it should "treat dvszwmarrgswjxmb as naughty" in {
    Problem5.Part1Rules.isNice("dvszwmarrgswjxmb") shouldBe false
  }


  behavior of "countNice with the rules from part 1"

  it should "count the right number of nice words" in {
    val count = Problem5.countNice(Iterator(
      "ugknbfddgicrmopn",
      "aaa",
      "jchzalrnumimnmhp",
      "haegwjzuvuyypxyu",
      "dvszwmarrgswjxmb"
    ), Part1Rules)

    count shouldBe 2
  }
}
