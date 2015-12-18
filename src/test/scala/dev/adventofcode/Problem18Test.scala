package dev.adventofcode

import dev.adventofcode.Problem18.Board
import org.scalatest.{Matchers, FlatSpec}

class Problem18Test extends FlatSpec with Matchers {

  val board1 = Board(List(
    List(false, true, false, true, false, true),
    List(false, false, false, true, true, false),
    List(true, false, false, false, false, true),
    List(false, false, true, false, false, false),
    List(true, false, true, false, false, true),
    List(true, true, true, true, false, false)
  ))

  val board2 = Board(List(
    List(false, false, true, true, false, false),
    List(false, false, true, true, false, true),
    List(false, false, false, true, true, false),
    List(false, false, false, false, false, false),
    List(true, false, false, false, false, false),
    List(true, false, true, true, false, false)
  ))

  behavior of "Input"

  it should "parse a board" in {
    val lines = List(
      ".#.#.#",
      "...##.",
      "#....#",
      "..#...",
      "#.#..#",
      "####.."
    )

    Problem18.parseLines(lines.toIterator) shouldEqual board1
  }

  behavior of "board"

  it should "move to the next step correctly" in {
    board1.step() shouldEqual board2
  }

  it should "have the right number of lights on" in {
    board1.numOn shouldEqual 15
  }

  it should "calculate the number of neighbors on correctly" in {
    board1.neighborsOn(0, 0) shouldEqual 1
    board1.neighborsOn(0, 1) shouldEqual 0
    board1.neighborsOn(4, 1) shouldEqual 6
    board1.neighborsOn(2, 0) shouldEqual 0
  }
}
