package dev.adventofcode

import scala.collection.immutable.NumericRange
import scala.collection.mutable.ArrayBuffer

object Problem20 {

  def main(args: Array[String]) {
    val maxHouseNum = 36000000
    val presents: ArrayBuffer[Int] = ArrayBuffer.fill(maxHouseNum/10)(0)

    (1 until maxHouseNum).foreach { house =>
      NumericRange(house, maxHouseNum/10, house).foreach(i => presents(i) = presents(i) + house*10)
    }

    val firstHouse = presents.indexWhere( presents => presents >= maxHouseNum )
    System.out.println(s"First house: $firstHouse")

    val presents2: ArrayBuffer[Int] = ArrayBuffer.fill(maxHouseNum/10)(0)
    (1 until maxHouseNum).foreach { house =>
      (1 to 50).foreach { i =>
        val index: Int = house * i
        if (index < maxHouseNum/10) {
          presents2(index) = presents2(index) + house * 11
        }
      }
    }

    val part2House = presents2.indexWhere( presents => presents >= maxHouseNum )
    System.out.println(s"Part 2 house: $part2House")
  }
}
