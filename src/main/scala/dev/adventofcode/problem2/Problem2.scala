package dev.adventofcode.problem2

import java.io.File

import com.google.common.io.Resources

import scala.io.{BufferedSource, Source}

object Problem2 {

  case class Present(length: Int, width: Int, height: Int) {
    lazy val sortedSides = List(length, width, height).sorted

    def smallestSide: Int = sortedSides.head * sortedSides(1)
    def surfaceArea: Int = 2*length*width + 2*width*height + 2*height*length
    def requiredPaper: Int = surfaceArea + smallestSide

    def shortestPath = 2*sortedSides.head + 2*sortedSides(1)
    def bow: Int = length*width*height
    def requiredRibbon: Int = shortestPath + bow
  }

  object StringToPresent {
    val regex = """(\d+)x(\d+)x(\d+)""".r // e.g. 7x4x10

    def apply(str: String): Present = str match {
      case regex(length, width, height) => Present(length.toInt, width.toInt, height.toInt)
    }
  }

  def totalPaper(presents: Iterator[Present]) = presents.foldLeft(0) { (sum, present) => sum + present.requiredPaper }
  def totalRibbon(presents: Iterator[Present]) = presents.foldLeft(0) { (sum, present) => sum + present.requiredRibbon }

  def main (args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem2/input").toURI)

    val presents = Source.fromFile(inputFile).getLines().map(line => StringToPresent(line)).toList

    System.out.println(s"Total paper: ${totalPaper(presents.iterator)}")
    System.out.println(s"Total ribbon: ${totalRibbon(presents.iterator)}")
  }
}
