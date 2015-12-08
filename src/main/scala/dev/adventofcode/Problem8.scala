package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem8 {

  def codeCount(str: String): Int = str.length

  val linePattern = """^"([a-z0-9"\\]+)"$""".r
  def memoryCount(str: String): Int = str match {
    case linePattern(letters) =>
      letters
        .replaceAll("""\\"""", " ")
        .replaceAll("""\\\\""", " ")
        .replaceAll("""\\x[0-9a-f]{2}""", " ")
        .length
    case _ => 0
  }

  def encodedCount(str: String): Int = str
    .replaceAll("""\\"""", "1234")
    .replaceAll("""\\\\""", "1234")
    .replaceAll("""\\x[0-9a-f]{2}""", "12345")
    .length + 4 // 4 more characters to escape the outside quotes

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem8/input").toURI)

    val part1Sum = Source.fromFile(inputFile).getLines().foldLeft(0) { (sum, line) =>
      sum + (Problem8.codeCount(line) - Problem8.memoryCount(line))
    }

    System.out.println(s"Code - memory characters: $part1Sum")

    val part2Sum = Source.fromFile(inputFile).getLines().foldLeft(0) { (sum, line) =>
      sum + (Problem8.encodedCount(line) - Problem8.codeCount(line))
    }

    System.out.println(s"Encoded - code characters: $part2Sum")
  }
}
