package dev.adventofcode

object Problem17 {
  val containers = List(11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32, 36, 15, 11, 46, 26, 28, 1, 19, 3)

  def main(args: Array[String]) {
    val combinations = (1 to containers.size).map { numInCombination =>
      containers.zipWithIndex.combinations(numInCombination).count {
        numsWithIndexes => numsWithIndexes.map(_._1).sum == 150
      }
    }.sum

    System.out.println(s"$combinations combinations of containers")

    val minContainers = (1 to containers.size).find { numInCombination =>
      containers.zipWithIndex.combinations(numInCombination).exists {
        numsWithIndexes => numsWithIndexes.map(_._1).sum == 150
      }
    }.sum

    val waysToUseMinContainers = containers.zipWithIndex.combinations(minContainers).count {
      numsWithIndexes => numsWithIndexes.map(_._1).sum == 150
    }

    System.out.println(s"$waysToUseMinContainers ways to use $minContainers")
  }
}
