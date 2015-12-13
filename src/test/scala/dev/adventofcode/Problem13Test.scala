package dev.adventofcode

import dev.adventofcode.Problem13.{Direction, Edge}
import org.scalatest.{FlatSpec, Matchers}

class Problem13Test extends FlatSpec with Matchers {

  val exampleDirections = Map(
    Direction("Alice", "Bob") -> 54,
    Direction("Alice", "Carol") -> -79,
    Direction("Alice", "David") -> -2,
    Direction("Bob", "Alice") -> 83,
    Direction("Bob", "Carol") -> -7,
    Direction("Bob", "David") -> -63,
    Direction("Carol", "Alice") -> -62,
    Direction("Carol", "Bob") -> 60,
    Direction("Carol", "David") -> 55,
    Direction("David", "Alice") -> 46,
    Direction("David", "Bob") -> -7,
    Direction("David", "Carol") -> 41
  )

  val exampleLines = List(
    "Alice would gain 54 happiness units by sitting next to Bob.",
    "Alice would lose 79 happiness units by sitting next to Carol.",
    "Alice would lose 2 happiness units by sitting next to David.",
    "Bob would gain 83 happiness units by sitting next to Alice.",
    "Bob would lose 7 happiness units by sitting next to Carol.",
    "Bob would lose 63 happiness units by sitting next to David.",
    "Carol would lose 62 happiness units by sitting next to Alice.",
    "Carol would gain 60 happiness units by sitting next to Bob.",
    "Carol would gain 55 happiness units by sitting next to David.",
    "David would gain 46 happiness units by sitting next to Alice.",
    "David would lose 7 happiness units by sitting next to Bob.",
    "David would gain 41 happiness units by sitting next to Carol."
  )

  behavior of "parsing"

  it should "parse a gain line correctly" in {
    val line = "Alice would gain 26 happiness units by sitting next to Carol."
    Problem13.parseLine(line) shouldEqual Some(Edge("Alice", "Carol", 26))
  }

  it should "parse a loss line correctly" in {
    val line = "Alice would lose 82 happiness units by sitting next to David."
    Problem13.parseLine(line) shouldEqual Some(Edge("Alice", "David", -82))
  }

  it should "parse a handful of lines correctly" in {
    Problem13.parseLines(exampleLines.iterator) shouldEqual exampleDirections
  }

  it should "Convert directions to a list of names" in {
    val expectedNames = List("Alice", "Bob", "Carol", "David")

    Problem13.extractNames(exampleDirections) should contain theSameElementsAs expectedNames
  }

  behavior of "happiness"

  it should "Calculate happiness" in {
    val names = List("David", "Alice", "Bob", "Carol")
    Problem13.calculateHappiness(names, exampleDirections) shouldEqual 330
  }

  it should "Calculate the maximum happiness" in {
    Problem13.maximalHappiness(exampleDirections) shouldEqual 330
  }
}
