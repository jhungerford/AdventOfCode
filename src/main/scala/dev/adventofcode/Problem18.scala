package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem18 {

  def parseLines(lines: Iterator[String]): Board = {
    val lights = lines.map { line =>
      line.map {
        case '.' => false
        case '#' => true
      }.toList
    }.toList

    Board(lights)
  }

  case class Board(lights: List[List[Boolean]]) {
    def numOn: Int = lights.map { line => line.count(light => light) }.sum

    def step(): Board = {
      val newLights: List[List[Boolean]] = lights.indices.map { rowNum =>
        lights(rowNum).indices.map { colNum =>
          val neighbors: Int = neighborsOn(rowNum, colNum)

          lights(rowNum)(colNum) match {
            // A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
            case true if neighbors == 2 || neighbors == 3 => true
            case true => false

            // A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
            case false if neighbors == 3 => true
            case false => false
          }
        }.toList
      }.toList

      Board(newLights)
    }

    def neighborsOn(rowNum: Int, colNum: Int): Int = {
      def kernel1(num: Int, max: Int): Range = Math.max(num - 1, 0) to Math.min(num + 1, max - 1)

      val kernelSum = kernel1(rowNum, lights.length).map { row =>
        kernel1(colNum, lights(row).length).count {
          col => lights(row)(col)
        }
      }.sum

      lights(rowNum)(colNum) match {
        case true => kernelSum - 1
        case false => kernelSum
      }
    }
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem18/input").toURI)
    val board = parseLines(Source.fromFile(inputFile).getLines())

    val boardAfter100Steps = (1 to 100).foldLeft(board) { (board, _) => board.step() }

    System.out.println(s"Board has ${boardAfter100Steps.numOn} lights on after 100 steps")
  }
}
