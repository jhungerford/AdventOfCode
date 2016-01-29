package dev.adventofcode

object Problem24 {

  case class Sleigh(group1: List[Int], group2: List[Int], group3: List[Int]) {
    def isWeightEven: Boolean = (group1.sum == group2.sum) && (group2.sum == group3.sum)
  }

  def allArrangements(packages: List[Int]): List[Sleigh] = {
    packages.permutations.flatMap { permutation =>
      val stuff = for {
        divider1 <- 1 to permutation.length - 2
        divider2 <- divider1 + 1 to permutation.length - 1
      } yield Sleigh(permutation.slice(0, divider1), permutation.slice(divider1, divider2), permutation.slice(divider2, permutation.length))
      stuff
    }.toList.filter( sleigh => sleigh.isWeightEven )
  }

  def main(args: Array[String]) {
    val packages = List(1, 2, 3, 5, 7, 13, 17, 19, 23, 29, 31, 37, 41, 43, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113)

    val arrangements: List[Sleigh] = allArrangements(packages)

    val sleighsByPackageCount: List[Sleigh] = arrangements.sortBy(sleigh => sleigh.group1.length)
    val lightestSleigh = sleighsByPackageCount
      .filter(sleigh => sleigh.group1.length == sleighsByPackageCount.head.group1.length)
      .sortBy(sleigh => sleigh.group1.product).head

    System.out.println(s"Lightest sleigh (part1): $lightestSleigh")
  }
}
