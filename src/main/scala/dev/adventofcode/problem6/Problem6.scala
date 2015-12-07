package dev.adventofcode.problem6

import java.io.File

import com.google.common.io.Resources

import scala.io.Source
import scala.util.matching.Regex

object Problem6 {

  val onRegex = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
  val offRegex = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
  val toggleRegex = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

  trait InstructionHandler[T] {
    def on(lights: Array[Array[T]], x1: Int, y1: Int, x2: Int, y2: Int): Unit
    def off(lights: Array[Array[T]], x1: Int, y1: Int, x2: Int, y2: Int): Unit
    def toggle(lights: Array[Array[T]], x1: Int, y1: Int, x2: Int, y2: Int): Unit

    def solve(lines: Iterator[String]): Int
  }

  object Part1InstructionHandler extends InstructionHandler[Boolean] {
    override def on(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = true
        }
      }
    }

    override def off(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = false
        }
      }
    }

    override def toggle(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = !lights(y)(x)
        }
      }
    }

    override def solve(lines: Iterator[String]): Int = {
      val lights = Array.ofDim[Boolean](1000, 1000)
      (0 to 999).foreach{ x =>
        (0 to 999).foreach{ y =>
          lights(y)(x) = false
        }
      }

      lines.foreach {
        case onRegex(x1, y1, x2, y2) => Part1InstructionHandler.on(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case offRegex(x1, y1, x2, y2) => Part1InstructionHandler.off(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case toggleRegex(x1, y1, x2, y2) => Part1InstructionHandler.toggle(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      }

      val numOn = lights.foldLeft(0) { (sum, row) =>
        sum + row.foldLeft(0) { (rowSum, on) =>
          rowSum + (if (on) 1 else 0)
        }
      }

      numOn
    }
  }

  object Part2InstructionHandler extends InstructionHandler[Int] {
    override def on(lights: Array[Array[Int]], x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = lights(y)(x) + 1
        }
      }
    }

    override def off(lights: Array[Array[Int]], x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = if (lights(y)(x) <= 1) 0 else lights(y)(x) - 1
        }
      }
    }

    override def toggle(lights: Array[Array[Int]], x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
      (x1 to x2).foreach { x =>
        (y1 to y2).foreach { y =>
          lights(y)(x) = lights(y)(x) + 2
        }
      }
    }

    override def solve(lines: Iterator[String]): Int = {
      val lights = Array.ofDim[Int](1000, 1000)
      (0 to 999).foreach{ x =>
        (0 to 999).foreach{ y =>
          lights(y)(x) = 0
        }
      }

      lines.foreach {
        case onRegex(x1, y1, x2, y2) => on(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case offRegex(x1, y1, x2, y2) => off(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case toggleRegex(x1, y1, x2, y2) => toggle(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      }

      lights.foldLeft(0) { (sum, row) => sum + row.sum }
    }
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem6/input").toURI)

    val part1Num = Part1InstructionHandler.solve(Source.fromFile(inputFile).getLines())
    val part2Num = Part2InstructionHandler.solve(Source.fromFile(inputFile).getLines())

    System.out.println(s"$part1Num lights are on after the instructions in part 1")
    System.out.println(s"$part2Num lights are on after the instructions in part 2")
  }
}
