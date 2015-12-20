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

  val calibrationReplacements = Map(
    "H" -> List("HO", "OH"),
    "O" -> List("HH")
  )

  it should "count the number of replacements in the example correctly" in {
    Problem19.generateReplacements("HOH", calibrationReplacements) shouldEqual Set(
      "HOOH", "HOHO", "OHOH", "HHHH"
    )
  }

  it should "parse replacements" in {
    val lines: List[String] = List(
      "H => HO",
      "H => OH",
      "O => HH"
    )

    Problem19.parseReplacements(lines) shouldEqual calibrationReplacements
  }

  val fromElectronReplacements = Map(
    "e" -> List("H", "O"),
    "O" -> List("HH"),
    "H" -> List("OH", "HO")
  )

  it should "calculate the number of steps to make HOH" in {
    Problem19.stepsToMolecule("HOH", fromElectronReplacements) shouldEqual 3
  }

  it should "calculate the number of steps to make HOHOHO" in {
    Problem19.stepsToMolecule("HOHOHO", fromElectronReplacements) shouldEqual 6
  }
}
