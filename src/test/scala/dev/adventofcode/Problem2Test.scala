package dev.adventofcode

import dev.adventofcode.Problem2.{Present, StringToPresent}
import org.scalatest.{FlatSpec, Matchers}

class Problem2Test extends FlatSpec with Matchers {

  behavior of "StringToPresent"

  it should "parse 20x3x11 correctly" in {
    StringToPresent("20x3x11") shouldEqual Present(20, 3, 11)
  }


  behavior of "smallest side"

  it should "find the smallest side of a 1x1x10 package correctly" in {
    Present(1, 1, 10).smallestSide shouldEqual 1
  }

  it should "find the smallest side of a 30x3x13 present correctly" in {
    Present(30, 3, 13).smallestSide shouldEqual 39
  }


  behavior of "Present required paper"

  it should "calculate a 2x3x4 present correctly" in {
    Present(2, 3, 4).requiredPaper shouldEqual 58
  }

  it should "calculate a 1x1x10 present correctly" in {
    Present(1, 1, 10).requiredPaper shouldEqual 43
  }

  it should "calculate a 30x3x13 present correctly" in {
    Present(30, 3, 13).requiredPaper shouldEqual 1077
  }


  behavior of "Total paper"

  it should "Add up the paper required for several presents correctly" in {
    val presents = Iterator[Present](
      Present(1, 1, 10),
      Present(2, 3, 4)
    )

    Problem2.totalPaper(presents) shouldEqual 101
  }


  behavior of "present ribbon"

  it should "get the right ribbon length for a 2x3x4 present" in {
    Present(2, 3, 4).requiredRibbon shouldEqual 34
  }

  it should "get the right ribbon length for a 1x1x10 present" in {
    Present(1, 1, 10).requiredRibbon shouldEqual 14
  }


  behavior of "total ribbon"

  it should "add up the ribbon required for several presents correctly" in {
    val presents = Iterator[Present](
      Present(1, 1, 10),
      Present(2, 3, 4)
    )

    Problem2.totalRibbon(presents) shouldEqual 48
  }
}
