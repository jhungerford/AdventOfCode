package dev.adventofcode

object Problem20 {
  def numPresents(house: Int): Int = {
    (1 to house).filter( i => house % i == 0 ).map(i => i * 10).sum
  }

  def main(args: Array[String]) {
    val minPresents = 36000000

    val firstHouse = Stream.from(1).collectFirst {
      case house if numPresents(house) >= minPresents => house
    }

    System.out.println(s"First house to get >$minPresents presents: $firstHouse")
  }
}
