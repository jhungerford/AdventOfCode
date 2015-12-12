package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem12 {


  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem12/input").toURI)
    val jsonStr = Source.fromFile(inputFile).getLines().next()

    val numberRegex = """(-?[0-9]+)""".r
    val sum = numberRegex.findAllMatchIn(jsonStr)
      .map{numberMatch => numberMatch.group(1).toInt}
      .sum

    System.out.println(s"Sum of all numbers in the document: $sum")


  }
}
