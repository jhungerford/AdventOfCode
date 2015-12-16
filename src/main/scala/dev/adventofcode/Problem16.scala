package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem16 {

  case class Sue(number: Int, info: Map[String, Int])

  val sueRegex = """Sue (\d+): ([a-z]+): (\d+), ([a-z]+): (\d+), ([a-z]+): (\d+)""".r
  def parseSues(lines: Iterator[String]): List[Sue] = {
    lines.toList.flatMap {
      case sueRegex(number, key1, value1, key2, value2, key3, value3) => Some(Sue(number.toInt, Map(
        key1 -> value1.toInt,
        key2 -> value2.toInt,
        key3 -> value3.toInt
      )))
      case _ => None
    }
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem16/input").toURI)
    val sues = parseSues(Source.fromFile(inputFile).getLines())

    val clue = Map(
      "children" -> 3,
      "cats" -> 7,
      "samoyeds" -> 2,
      "pomeranians" -> 3,
      "akitas" -> 0,
      "vizslas" -> 0,
      "goldfish" -> 5,
      "trees" -> 3,
      "cars" -> 2,
      "perfumes" -> 1
    )

    val matchingSue = sues.find { sue =>
      sue.info.forall {
        case (sueKey, sueValue) => clue(sueKey) == sueValue
      }
    }

    System.out.println(s"Exact matching sue: $matchingSue")

    val directionsSue = sues.find { sue =>
      sue.info.forall {
        case ("cats", sueValue) => sueValue > clue("cats")
        case ("trees", sueValue) => sueValue > clue("trees")
        case ("pomeranians", sueValue) => sueValue < clue("pomeranians")
        case ("goldfish", sueValue) => sueValue < clue("goldfish")
        case (sueKey, sueValue) => sueValue == clue(sueKey)
      }
    }

    System.out.println(s"Reading the directions matching sue: $directionsSue")
  }
}
