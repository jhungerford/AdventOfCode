package dev.adventofcode.problem1

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object EnterBasement {
  def apply(iterator: Iterator[Char]): Int = {
    // This is a contrived implementation, but I want to play with (potentially) infinite streams.
    // Convert the instructions to up/down values,
    // Convert them to a stream of the current floor by summing the previous instructions
    // And grab the first basement index
    iterator.toStream.map {
      case '(' => 1
      case ')' => -1
      case _ => ???
    }.scanLeft(0)( _ + _ ).indexWhere(sum => sum < 0)
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem1/input").toURI)
    val source = Source.fromFile(inputFile)

    try {
      System.out.println(s"Basement index: ${EnterBasement(source)}")
    } finally {
      source.close()
    }
  }
}
