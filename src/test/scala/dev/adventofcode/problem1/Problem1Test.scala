package dev.adventofcode.problem1

import org.scalatest.{Matchers, FlatSpec}

class Problem1Test extends FlatSpec with Matchers {

  behavior of "final floor sample input"

  it should "get floor 0 for (()) and ()()" in {
    FinalFloor("(())".iterator) shouldEqual 0
    FinalFloor("()()".iterator) shouldEqual 0
  }

  it should "get floor 3 for ((( and (()(()(" in {
    FinalFloor("(((".iterator) shouldEqual 3
    FinalFloor("(()(()(".iterator) shouldEqual 3
  }

  it should "get floor 3 for ))(((((" in {
    FinalFloor("))(((((".iterator) shouldEqual 3
  }

  it should "get -1 for ()) and ))(" in {
    FinalFloor("())".iterator) shouldEqual -1
    FinalFloor("))(".iterator) shouldEqual -1
  }

  it should "get floor -3 for ))) and )())())" in {
    FinalFloor(")))".iterator) shouldEqual -3
    FinalFloor(")())())".iterator) shouldEqual -3
  }

}
