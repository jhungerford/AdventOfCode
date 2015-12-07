package dev.adventofcode.problem6

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem6 {
  
  def on(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
    (x1 to x2).foreach{ x =>
      (y1 to y2).foreach{ y =>
        lights(y)(x) = true
      }
    }
  }

  def off(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
    (x1 to x2).foreach{ x =>
      (y1 to y2).foreach{ y =>
        lights(y)(x) = false
      }
    }

    lights
  }

  def toggle(lights: Array[Array[Boolean]], x1: Int, y1: Int, x2: Int, y2: Int) = {
    (x1 to x2).foreach{ x =>
      (y1 to y2).foreach{ y =>
        lights(y)(x) = ! lights(y)(x)
      }
    }

    lights
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem6/input").toURI)

    val onRegex = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
    val offRegex = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
    val toggleRegex = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

    val lights = Array.ofDim[Boolean](1000, 1000)

    (0 to 999).foreach{ x =>
      (0 to 999).foreach{ y =>
        lights(y)(x) = false
      }
    }

    Source.fromFile(inputFile).getLines().foreach {
      case onRegex(x1, y1, x2, y2) => on(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case offRegex(x1, y1, x2, y2) => off(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case toggleRegex(x1, y1, x2, y2) => toggle(lights, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }

    val numOn = lights.foldLeft(0) { (sum, row) =>
      sum + row.foldLeft(0) { (rowSum, on) =>
        rowSum + (if (on) 1 else 0)
      }
    }

    System.out.println(s"$numOn lights are on after the instructions")
  }
}
