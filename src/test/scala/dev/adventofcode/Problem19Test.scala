package dev.adventofcode

import org.scalatest.{Matchers, FlatSpec}

class Problem19Test extends FlatSpec with Matchers {
  behavior of "Problem 19"

  it should "find the indexes of H in HOOH correctly" in {
    Problem19.indexesOf("H", "HOOH") shouldEqual List(0, 3)
  }

  it should "find the indexes of O in HOOH correctly" in {
    Problem19.indexesOf("O", "HOOH") shouldEqual List(1, 2)
  }

  it should "count the number of replacements in the example correctly" in {
    val replacements = Map(
      "H" -> List("HO", "OH"),
      "O" -> List("HH")
    )

    val start = "HOH"

    Problem19.generateReplacements(start, replacements) shouldEqual Set(
      "HOOH", "HOHO", "OHOH", "HHHH"
    )
  }

  it should "parse replacements" in {
    val lines: List[String] = List(
      "H => HO",
      "H => OH",
      "O => HH"
    )

    val expected: Map[String, List[String]] = Map(
      "H" -> List("HO", "OH"),
      "O" -> List("HH")
    )

    Problem19.parseReplacements(lines) shouldEqual expected
  }
}
