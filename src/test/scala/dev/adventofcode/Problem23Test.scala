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
      JumpIfOneInstruction("a", 2),
      TripleInstruction("a"),
      IncrementInstruction("a")
    )
  }

  it should "set a to 2" in {
    val program = Problem23.parse(sampleLines.iterator)
    val result = Problem23.execute(Machine(0, 0, 0), program)

    result.a shouldEqual 2
  }

  behavior of "instructions"

  val machine = Machine(4, 9, 0)

  it should "half correctly" in {
    HalfInstruction("a").execute(machine) shouldEqual Machine(2, 9, 1)
    HalfInstruction("b").execute(machine) shouldEqual Machine(4, 4, 1)
  }

  it should "triple correctly" in {
    TripleInstruction("a").execute(machine) shouldEqual Machine(12, 9, 1)
    TripleInstruction("b").execute(machine) shouldEqual Machine(4, 27, 1)
  }

  it should "increment correctly" in {
    IncrementInstruction("a").execute(machine) shouldEqual Machine(5, 9, 1)
    IncrementInstruction("b").execute(machine) shouldEqual Machine(4, 10, 1)
  }

  it should "jump offset correctly" in {
    JumpInstruction(2).execute(machine) shouldEqual Machine(4, 9, 2)
  }

  it should "jump if even correctly" in {
    JumpIfEvenInstruction("a", -2).execute(machine) shouldEqual Machine(4, 9, -2)
    JumpIfEvenInstruction("b", -2).execute(machine) shouldEqual Machine(4, 9, 1)
  }

  it should "jump if one correctly" in {
    val oneMachine = Machine(1, 2, 3)
    JumpIfOneInstruction("a", -2).execute(machine) shouldEqual Machine(4, 9, 1)
    JumpIfOneInstruction("a", -2).execute(oneMachine) shouldEqual Machine(1, 2, 1)
    JumpIfOneInstruction("b", -2).execute(oneMachine) shouldEqual Machine(1, 2, 4)
  }
}
