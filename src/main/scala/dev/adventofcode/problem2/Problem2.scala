package dev.adventofcode.problem2

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem2 {

  case class Present(length: Int, width: Int, height: Int) {
    def smallestSide: Int = {
      val sortedSides = List(length, width, height).sorted
      sortedSides.head * sortedSides(1)
    }

    def surfaceArea: Int = 2*length*width + 2*width*height + 2*height*length

    def requiredPaper: Int = surfaceArea + smallestSide
  }

  object StringToPresent {
    val regex = """(\d+)x(\d+)x(\d+)""".r // e.g. 7x4x10

    def apply(str: String): Present = str match {
      case regex(length, width, height) => Present(length.toInt, width.toInt, height.toInt)
    }
  }

  def totalPaper(presents: Iterator[Present]) = presents.foldLeft(0) { (sum, present) => sum + present.requiredPaper }

  def main (args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem2/input").toURI)

    val presents = Source.fromFile(inputFile).getLines()
      .map(line => StringToPresent(line))

    System.out.println(s"Total paper: ${totalPaper(presents)}")
  }
}
