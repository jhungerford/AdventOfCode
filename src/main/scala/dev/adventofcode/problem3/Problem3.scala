package dev.adventofcode.problem3

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem3 {

  case class Position(x: Int, y: Int) {
    def move(direction: Char) = direction match {
      case '^' => Position(x, y+1)
      case 'v' => Position(x, y-1)
      case '<' => Position(x-1, y)
      case '>' => Position(x+1, y)
    }
  }

  case class EvenOddPositions(even: Position, odd: Position)

  def uniqueHouseCount(instructions: Iterator[Char]): Int = {
    instructions.scanLeft(Position(0,0)) { (pos, direction) =>
      pos.move(direction)
    }.toSet.size
  }

  def evenOddHouseCount(instructions: Iterator[Char]): Int = {
    // a different actor handles the even and odd instructions
    val allPositions = instructions.zipWithIndex.scanLeft(EvenOddPositions(Position(0,0), Position(0,0))) {
      case (positions, (direction, index)) if index % 2 == 0 => positions.copy(even = positions.even.move(direction))
      case (positions, (direction, index)) if index % 2 == 1 => positions.copy(odd = positions.odd.move(direction))
    }

    // Collapse the positions - if even and odd visit Position(0,0), count it as one house.
    allPositions.flatMap(positions => Iterable(positions.even, positions.odd)).toSet.size
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem3/input").toURI)

    System.out.println(s"Unique houses: ${uniqueHouseCount(Source.fromFile(inputFile))}")
    System.out.println(s"Even/odd houses: ${evenOddHouseCount(Source.fromFile(inputFile))}")
  }
}
