package dev.adventofcode.problem1

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object FinalFloor {
  def apply(iterator: Iterator[Char]): Int = {
    iterator.foldLeft(0) { (floor, instruction) =>
      val direction = instruction match {
        case '(' => 1
        case ')' => -1
        case _ => ???
      }

      floor + direction
    }
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem1/input").toURI)
    val source = Source.fromFile(inputFile)

    try {
      val finalFloor = FinalFloor(source)
      System.out.println(s"Final floor: $finalFloor")
    } finally {
      source.close()
    }
  }
}
