package dev.adventofcode

import java.io.File

import com.google.common.io.Resources
import dev.adventofcode.Problem7.Gates.Gate

import scala.io.Source
import scala.util.matching.Regex

object Problem7 {

  val SHORT_MASK = 0xFFFF

  // signal: 16 bit (0 to 65535) - unsigned short

  object Gates {

    sealed trait Gate {
      def evaluate(circuit: Circuit): Int
    }

    // ValueGate: signal -> wire
    case class ValueGate(value: Int) extends Gate {
      override def evaluate(circuit: Circuit): Int = value
    }

    // PassThroughGate: wire -> wire
    case class PassThroughGate(wire: String) extends Gate {
      override def evaluate(circuit: Circuit): Int = circuit(wire)
    }

    // AndGate: wire AND wire -> wire
    case class AndGate(wire1: String, wire2: String) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        (circuit(wire1) & circuit(wire2)) & SHORT_MASK
    }

    // OneAndGate: 1 AND wire -> wire
    case class OneAndGate(wire: String) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        (1 & circuit(wire)) & SHORT_MASK
    }

    // OrGate: wire OR wire -> wire
    case class OrGate(wire1: String, wire2: String) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        (circuit(wire1) | circuit(wire2)) & SHORT_MASK
    }

    // NotGate: NOT wire -> wire
    case class NotGate(wire: String) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        ~circuit(wire) & SHORT_MASK
    }

    // RShiftGate: wire RSHIFT amount -> wire
    case class RShiftGate(wire: String, amount: Int) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        (circuit(wire) >>> amount) & SHORT_MASK
    }

    // LShiftGate: wire LSHIFT amount -> wire
    case class LShiftGate(wire: String, amount: Int) extends Gate {
      override def evaluate(circuit: Circuit): Int =
        (circuit(wire) << amount) & SHORT_MASK
    }

    val valuePattern = """(\d+)""".r
    val passThroughPattern = """([a-z]+)""".r
    val andPattern = """([a-z]+) AND ([a-z]+)""".r
    val oneAndPattern = """1 AND ([a-z]+)""".r
    val orPattern = """([a-z]+) OR ([a-z]+)""".r
    val notPattern = """NOT ([a-z]+)""".r
    val lShiftPattern = """([a-z]+) LSHIFT ([0-9]+)""".r
    val rShiftPattern = """([a-z]+) RSHIFT ([0-9]+)""".r

    def parse(gateStr: String): Gate = gateStr match {
      case valuePattern(value) => ValueGate(value.toInt)
      case passThroughPattern(wire) => PassThroughGate(wire)
      case andPattern(wire1, wire2) => AndGate(wire1, wire2)
      case oneAndPattern(wire) => OneAndGate(wire)
      case orPattern(wire1, wire2) => OrGate(wire1, wire2)
      case notPattern(wire) => NotGate(wire)
      case lShiftPattern(wire, value) => LShiftGate(wire, value.toInt)
      case rShiftPattern(wire, value) => RShiftGate(wire, value.toInt)
    }
  }

  val wirePattern = """(.+) -> ([a-z]+)""".r
  def linesToTable(lines: Iterator[String]): Map[String, Gate] = lines.foldLeft(Map.empty[String, Gate]) { (map, line) => line match {
      case wirePattern(gateStr, wire) => map + (wire -> Gates.parse(gateStr))
      case _ => map
    }
  }

  class Circuit(table: Map[String, Gate]) {
    val cache = scala.collection.mutable.Map.empty[String, Int]
    def apply(gate: String): Int = cache.getOrElseUpdate(gate, table(gate).evaluate(this))
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem7/input").toURI)

    val circuit = new Circuit(linesToTable(Source.fromFile(inputFile).getLines()))

    System.out.println(s"Output of wire a: ${circuit("a")}")
  }
}
