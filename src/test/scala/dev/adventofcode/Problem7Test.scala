package dev.adventofcode

import dev.adventofcode.Problem7.{Circuit, Gates}
import dev.adventofcode.Problem7.Gates._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class Problem7Test extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  behavior of "simple circuit"

  val table: Map[String, Gate] = Map(
    "x" -> ValueGate(123),
    "y" -> ValueGate(456),
    "d" -> AndGate("x", "y"),
    "e" -> OrGate("x", "y"),
    "f" -> LShiftGate("x", 2),
    "g" -> RShiftGate("y", 2),
    "h" -> NotGate("x"),
    "i" -> NotGate("y")
  )

  val circuit = new Circuit(table)

  it should "parse the lines correctly" in {
    Problem7.linesToTable(Iterator[String](
      "123 -> x",
      "456 -> y",
      "x AND y -> d",
      "x OR y -> e",
      "x LSHIFT 2 -> f",
      "y RSHIFT 2 -> g",
      "NOT x -> h",
      "NOT y -> i"
    )) shouldEqual table
  }

  val expectedResults = Table(
    ("wire", "output"),
    ("d", 72),
    ("e", 507),
    ("f", 492),
    ("g", 114),
    ("h", 65412),
    ("i", 65079),
    ("x", 123),
    ("y", 456)
  )

  it should "get all of the expected results" in {
    forAll(expectedResults) { (wire: String, expected: Int) =>
      circuit(wire) shouldEqual expected
    }
  }

  behavior of "Gates.parse"

  it should "parse value correctly" in {
    Gates.parse("123") shouldEqual ValueGate(123)
  }

  it should "parse passthrough correctly" in {
    Gates.parse("x") shouldEqual PassThroughGate("x")
  }

  it should "parse and correctly" in {
    Gates.parse("x AND y") shouldEqual AndGate("x", "y")
  }

  it should "parse or correctly" in {
    Gates.parse("x OR y") shouldEqual OrGate("x", "y")
  }

  it should "parse lshift correctly" in {
    Gates.parse("x LSHIFT 2") shouldEqual LShiftGate("x", 2)
  }

  it should "parse rshift correctly" in {
    Gates.parse("y RSHIFT 2") shouldEqual RShiftGate("y", 2)
  }

  it should "parse not correctly" in {
    Gates.parse("NOT x") shouldEqual NotGate("x")
  }








}
