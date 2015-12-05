package dev.adventofcode.problem5

import dev.adventofcode.problem5.Problem5.{Part2Rules, Part1Rules}
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


  behavior of "Part2Rules.isNice"

  it should "treat qjhvhtzxzqqjkmpb as nice" in {
    Problem5.Part2Rules.isNice("qjhvhtzxzqqjkmpb") shouldBe true
  }

  it should "treat xxyxx as nice" in {
    Problem5.Part2Rules.isNice("xxyxx") shouldBe true
  }

  it should "treat uurcxstgmygtbstg as naughty" in {
    Problem5.Part2Rules.isNice("uurcxstgmygtbstg") shouldBe false
  }

  it should "treat ieodomkazucvgmuy as naughty" in {
    Problem5.Part2Rules.isNice("ieodomkazucvgmuy") shouldBe false
  }


  behavior of "Part2Rules.hasNonOverlapingRepeatedPair"

  it should "say yes to xxyxx" in {
    Part2Rules.hasNonOverlappingRepeatedPair("xxyxx") shouldBe true
  }

  it should "say no to aaapple" in {
    Part2Rules.hasNonOverlappingRepeatedPair("aaapple") shouldBe false
  }

  it should "say yes to aaaxxyxx" in {
    Part2Rules.hasNonOverlappingRepeatedPair("aaaxxyxx") shouldBe true
  }

  it should "say yes to xyxy" in {
    Part2Rules.hasNonOverlappingRepeatedPair("xyxy") shouldBe true
  }


  behavior of "Part2Rules.hasRepeatedCharWithASeparator"

  it should "say yes to xyx" in {
    Part2Rules.hasRepeatedCharWithASeparator("xyx") shouldBe true
  }

  it should "say yes to aaa" in {
    Part2Rules.hasRepeatedCharWithASeparator("aaa") shouldBe true
  }

  it should "say yes to canal" in {
    Part2Rules.hasRepeatedCharWithASeparator("canal") shouldBe true
  }

  it should "say no to apple" in {
    Part2Rules.hasRepeatedCharWithASeparator("apple") shouldBe false
  }
}
