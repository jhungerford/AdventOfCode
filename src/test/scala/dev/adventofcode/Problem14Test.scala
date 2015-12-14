package dev.adventofcode

import dev.adventofcode.Problem14.Reindeer
import org.scalatest.{Matchers, FlatSpec}

class Problem14Test extends FlatSpec with Matchers {

  private val comet: Reindeer = Reindeer("Comet", 14, 10, 127)
  private val dancer: Reindeer = Reindeer("Dancer", 16, 11, 162)

  behavior of "problem 14"

  it should "parse a line" in {
    val line = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
    Problem14.parseLine(line) shouldEqual Some(comet)
  }

  it should "calculate distance for comet" in {
    Problem14.distanceAfterSeconds(comet, 1000) shouldEqual 1120
  }


  it should "calculate distance for dancer" in {
    Problem14.distanceAfterSeconds(dancer, 1000) shouldEqual 1056
  }

  it should "calculate a winner" in {
    val reindeer = List( comet, dancer )
    Problem14.winningDistanceAfterSeconds(reindeer, 1000) shouldEqual 1120
  }

  it should "calculate a winner based on points" in {
    val reindeer = List( comet, dancer )
    Problem14.winningPointsAfterSeconds(reindeer, 1000) shouldEqual 689
  }

}
