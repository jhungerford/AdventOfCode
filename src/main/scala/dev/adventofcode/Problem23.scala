package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem23 {
  case class Machine(a: Int, b: Int, programCounter: Int)

  sealed trait Instruction {
    def execute(machine: Machine): Machine
  }

  // hlf r sets register r to half its current value, then continues with the next instruction.
  case class HalfInstruction(register: String) extends Instruction {
    override def execute(machine: Machine): Machine = register match {
      case "a" => machine.copy(a = machine.a / 2, programCounter = machine.programCounter + 1)
      case "b" => machine.copy(b = machine.b / 2, programCounter = machine.programCounter + 1)
      case _ => throw new IllegalStateException("Invalid register")
    }
  }

  // tpl r sets register r to triple its current value, then continues with the next instruction.
  case class TripleInstruction(register: String) extends Instruction {
    override def execute(machine: Machine): Machine = register match {
      case "a" => machine.copy(a = machine.a * 3, programCounter = machine.programCounter + 1)
      case "b" => machine.copy(b = machine.b * 3, programCounter = machine.programCounter + 1)
      case _ => throw new IllegalStateException("Invalid register")
    }
  }

  // inc r increments register r, adding 1 to it, then continues with the next instruction.
  case class IncrementInstruction(register: String) extends Instruction {
    override def execute(machine: Machine): Machine = register match {
      case "a" => machine.copy(a = machine.a + 1, programCounter = machine.programCounter + 1)
      case "b" => machine.copy(b = machine.b + 1, programCounter = machine.programCounter + 1)
      case _ => throw new IllegalStateException("Invalid register")
    }
  }

  // jmp offset is a jump; it continues with the instruction offset away relative to itself.
  case class JumpInstruction(offset: Int) extends Instruction {
    override def execute(machine: Machine): Machine = machine.copy(programCounter = machine.programCounter + offset)
  }

  // jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
  case class JumpIfEvenInstruction(register: String, offset: Int) extends Instruction {
    override def execute(machine: Machine): Machine = register match {
      case "a" if machine.a % 2 == 0 => machine.copy(programCounter = machine.programCounter + offset)
      case "a" => machine.copy(programCounter = machine.programCounter + 1)

      case "b" if machine.b % 2 == 0 => machine.copy(programCounter = machine.programCounter + offset)
      case "b" => machine.copy(programCounter = machine.programCounter + 1)

      case _ => throw new IllegalStateException("Invalid register")
    }
  }

  // jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
  case class JumpIfOneInstruction(register: String, offset: Int) extends Instruction {
    override def execute(machine: Machine): Machine = register match {
      case "a" if machine.a == 1 => machine.copy(programCounter = machine.programCounter + offset)
      case "a" => machine.copy(programCounter = machine.programCounter + 1)

      case "b" if machine.b == 1 => machine.copy(programCounter = machine.programCounter + offset)
      case "b" => machine.copy(programCounter = machine.programCounter + 1)

      case _ => throw new IllegalStateException("Invalid register")
    }
  }

  val hlfRegex = """hlf ([ab])""".r
  val tplRegex = """tpl ([ab])""".r
  val incRegex = """inc ([ab])""".r
  val jmpRegex = """jmp ([+-][0-9]+)""".r
  val jieRegex = """jie ([ab]), ([+-][0-9]+)""".r
  val jioRegex = """jio ([ab]), ([+-][0-9]+)""".r

  def parse(lines: Iterator[String]): List[Instruction] = {
    lines.flatMap {
      case hlfRegex(register) => Some(HalfInstruction(register))
      case tplRegex(register) => Some(TripleInstruction(register))
      case incRegex(register) => Some(IncrementInstruction(register))
      case jmpRegex(offset) => Some(JumpInstruction(offset.toInt))
      case jieRegex(register, offset) => Some(JumpIfEvenInstruction(register, offset.toInt))
      case jioRegex(register, offset) => Some(JumpIfOneInstruction(register, offset.toInt))

      case blank if blank.isEmpty => None
      case invalid => throw new IllegalStateException(s"'$invalid' is not a valid instruction")
    }.toList
  }

  def execute(machine: Machine, program: List[Instruction]): Machine = {
    machine.programCounter < 0 || machine.programCounter >= program.length match {
      case true => machine
      case false =>
        val instruction: Instruction = program(machine.programCounter)
        val newMachine = instruction.execute(machine)
        System.out.println(s"$machine - $instruction -> $newMachine")
        execute(newMachine, program)
    }
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem23/input").toURI)

    val program = Problem23.parse(Source.fromFile(inputFile).getLines())

    val part1Result = execute(Machine(0, 0, 0), program)
    System.out.println(part1Result)

    val part2Result = execute(Machine(1, 0, 0), program)
    System.out.println(part2Result)
  }
}
