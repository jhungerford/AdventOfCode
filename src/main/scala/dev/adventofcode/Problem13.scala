package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem13 {

  case class Edge(from: String, to: String, happiness: Int)
  case class Direction(from: String, to: String)

  val lineRegex = """([a-zA-Z]+) would ([a-z]+) (\d+) happiness units by sitting next to ([a-zA-Z]+).""".r
  def parseLine(line: String): Option[Edge] = line match {
    case lineRegex(from, "lose", happinessStr, to) => Some(Edge(from, to, -1 * happinessStr.toInt))
    case lineRegex(from, "gain", happinessStr, to) => Some(Edge(from, to, happinessStr.toInt))
    case _ => None
  }

  def parseLines(lines: Iterator[String]): Map[Direction, Int] = {
    lines.flatMap(parseLine)
      .map{ edge => Direction(edge.from, edge.to) -> edge.happiness }
      .toMap
  }

  def extractNames(directions: Map[Direction, Int]): List[String] = {
    directions.keys
      .toSet[Direction]
      .flatMap{ direction => Set(direction.from, direction.to) }
      .toList
  }

  def calculateHappiness(names: List[String], directions: Map[Direction, Int]): Int = {
    // Calculate the full cycle of names - start with the last one in the list to wrap around
    names.foldLeft((names.last, 0)) { case ((prevName, sum), name) =>
      val happiness = directions(Direction(prevName, name)) + directions(Direction(name, prevName))
      (name, sum + happiness)
    }._2
  }

  def maximalHappiness(directions: Map[Direction, Int]): Int = {
    extractNames(directions)
      .permutations
      .map{ nameOrder => calculateHappiness(nameOrder, directions) }
      .max
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem13/input").toURI)
    val directions = parseLines(Source.fromFile(inputFile).getLines())

    System.out.println(s"Maximum happiness: ${maximalHappiness(directions)}")
  }
}
