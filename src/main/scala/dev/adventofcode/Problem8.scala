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

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem8/input").toURI)

    val sum = Source.fromFile(inputFile).getLines().foldLeft(0) { (sum, line) =>
      sum + (Problem8.codeCount(line) - Problem8.memoryCount(line))
    }

    System.out.println(s"Code - memory characters: $sum")
  }
}
