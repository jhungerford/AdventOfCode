package dev.adventofcode.problem3

import org.scalatest.{Matchers, FlatSpec}

class Problem3Test extends FlatSpec with Matchers {

  behavior of "unique house count"

  it should "be 2 for >" in {
    Problem3.uniqueHouseCount(">".iterator) shouldBe 2
  }

  it should "be 4 for ^>v<" in {
    Problem3.uniqueHouseCount("^>v<".iterator) shouldBe 4
  }

  it should "be 2 for ^v^v^v^v^v" in {
    Problem3.uniqueHouseCount("^v^v^v^v^v".iterator) shouldBe 2
  }


  behavior of "even odd house count"

  it should "be 3 for ^v" in {
    Problem3.evenOddHouseCount("^v".iterator) shouldBe 3
  }

  it should "be 3 for ^>v<" in {
    Problem3.evenOddHouseCount("^>v<".iterator) shouldBe 3
  }

  it should "be 11 for ^v^v^v^v^v" in {
    Problem3.evenOddHouseCount("^v^v^v^v^v".iterator) shouldBe 11
  }
}
