package dev.adventofcode

import dev.adventofcode.Problem1.{EnterBasement, FinalFloor}
import org.scalatest.{FlatSpec, Matchers}

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

  behavior of "enter basement"

  it should "get 1 for )" in {
    EnterBasement(")".iterator) shouldEqual 1
  }

  it should "get 5 for ()())" in {
    EnterBasement("()())".iterator) shouldEqual 5
  }
}
