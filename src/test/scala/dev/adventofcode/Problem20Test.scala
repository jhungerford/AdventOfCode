package dev.adventofcode

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}

class Problem20Test extends FlatSpec with TableDrivenPropertyChecks with Matchers  {

  behavior of "problem 20"

  val housePresents = Table(
    ("house", "presents"),
    (1, 10),
    (2, 30),
    (3, 40),
    (4, 70),
    (5, 60),
    (6, 120),
    (7, 80),
    (8, 150),
    (9, 130)
  )

  it should "calculate the right number of presents for a house" in {
    forAll(housePresents) { (house: Int, presents: Int) =>
      Problem20.numPresents(house) should equal(presents)
    }
  }

}
