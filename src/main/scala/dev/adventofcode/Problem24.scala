package dev.adventofcode

object Problem24 {

  def firstItem(target: Int, items: List[Int], count: Int): Option[List[Int]] = {
    items match {
      case item :: tail =>
        subsetsWithSum(target - item, items diff List(item), count - 1) match {
          case Some(list) => Some(list :+ item)
          case None => firstItem(target, tail, count)
        }
      case _ => None
    }
  }

  def subsetsWithSum(target: Int, items: List[Int], count: Int): Option[List[Int]] = {
    if (target == 0 && count == 0) {
      Some(List.empty)
    } else if (count == 0) {
      None
    } else {
      firstItem(target, items.filter(_ <= target), count)
    }
  }

  def firstGroupWithEvenWeight(packages: List[Int], groups: Int, count: Int = 1): List[Int] = {
    val weightPerGroup = packages.sum / groups

    subsetsWithSum(weightPerGroup, packages, count) match {
      case Some(subset) => subset
      case None => firstGroupWithEvenWeight(packages, groups, count + 1)
    }
  }

  def main(args: Array[String]) {
    val packages = List(1, 2, 3, 5, 7, 13, 17, 19, 23, 29, 31, 37, 41, 43, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113)

    val sleighThreeGroups = firstGroupWithEvenWeight(packages, 3)
    System.out.println(s"Lightest sleigh (part1): ${sleighThreeGroups.product} - $sleighThreeGroups")

    val sleighFourGroups = firstGroupWithEvenWeight(packages, 4)
    System.out.println(s"Lightest sleigh (part2): ${sleighFourGroups.product}")
  }
}
