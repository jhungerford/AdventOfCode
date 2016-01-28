package dev.adventofcode

import dev.adventofcode.Problem23._
import org.scalatest.{FlatSpec, Matchers}

class Problem23Test extends FlatSpec with Matchers {

  behavior of "sample program"

  val sampleLines = List(
    "inc a",
    "jio a, +2",
    "tpl a",
    "inc a"
  )

  it should "parse the sample input correctly" in {
    val program = Problem23.parse(sampleLines.iterator)

    program shouldEqual List(
      IncrementInstruction("a"),
      JumpIfOddInstruction("a", 2),
      TripleInstruction("a"),
      IncrementInstruction("a")
    )
  }

  it should "set a to 2" in {
    val program = Problem23.parse(sampleLines.iterator)
    val result = Problem23.execute(program)

    result.a shouldEqual 2
  }

  behavior of "instructions"

  it should "half correctly" in {
    val machine = Machine(4, 8, 0)
    HalfInstruction("a").execute(machine) shouldEqual Machine(2, 8, 1)
    HalfInstruction("b").execute(machine) shouldEqual Machine(4, 4, 1)
  }

  it should "triple correctly" in {
    val machine = Machine(4, 8, 0)
    TripleInstruction("a").execute(machine) shouldEqual Machine(12, 8, 1)
    TripleInstruction("b").execute(machine) shouldEqual Machine(4, 24, 1)
  }

  it should "increment correctly" in {
    val machine = Machine(4, 8, 0)
    IncrementInstruction("a").execute(machine) shouldEqual Machine(5, 8, 1)
    IncrementInstruction("b").execute(machine) shouldEqual Machine(4, 9, 1)
  }


}
