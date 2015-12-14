package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem14 {

  case class Reindeer(name: String, flyKms: Int, flySeconds: Int, restSeconds: Int)

  val lineRegex = """([a-zA-Z]+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r
  def parseLine(line: String): Option[Reindeer] = line match {
    case lineRegex(name, flyKmsStr, flySecondsStr, restSecondsStr) => Some(Reindeer(name, flyKmsStr.toInt, flySecondsStr.toInt, restSecondsStr.toInt))
    case _ => None
  }

  def distanceAfterSeconds(reindeer: Reindeer, seconds: Int): Int = {
    val cycleSeconds = reindeer.flySeconds + reindeer.restSeconds
    val fullCycles = seconds / cycleSeconds // Integer division - takes the floor to find complete cycles
    val remainingFlySeconds = Math.min(seconds % cycleSeconds, reindeer.flySeconds)

    val totalFlySeconds = fullCycles * reindeer.flySeconds + remainingFlySeconds
    totalFlySeconds * reindeer.flyKms
  }

  def winningDistanceAfterSeconds(reindeer: List[Reindeer], seconds: Int): Int = {
    reindeer.map{ deer => distanceAfterSeconds(deer, seconds) }.max
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem14/input").toURI)
    val reindeer: List[Reindeer] = Source.fromFile(inputFile).getLines().flatMap(parseLine).toList

    val seconds: Int = 2503
    val winningDistance = Problem14.winningDistanceAfterSeconds(reindeer, seconds)

    System.out.println(s"Winning distance after $seconds seconds: $winningDistance")
  }
}
