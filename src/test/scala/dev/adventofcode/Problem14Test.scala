package dev.adventofcode

import dev.adventofcode.Problem14.Reindeer
import org.scalatest.{Matchers, FlatSpec}

class Problem14Test extends FlatSpec with Matchers {

  behavior of "problem 14"

  it should "parse a line" in {
    val line = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
    val expected = Reindeer("Comet", 14, 10, 127)

    Problem14.parseLine(line) shouldEqual Some(expected)
  }

  it should "calculate distance for comet" in {
    val comet = Reindeer("Comet", 14, 10, 127)
    Problem14.distanceAfterSeconds(comet, 1000) shouldEqual 1120
  }

  it should "calculate distance for dancer" in {
    val comet = Reindeer("Dancer", 16, 11, 162)
    Problem14.distanceAfterSeconds(comet, 1000) shouldEqual 1056
  }

  it should "calculate a winner" in {
    val reindeer = List(
      Reindeer("Comet", 14, 10, 127),
      Reindeer("Dancer", 16, 11, 162)
    )

    Problem14.winningDistanceAfterSeconds(reindeer, 1000) shouldEqual 1120
  }

}
