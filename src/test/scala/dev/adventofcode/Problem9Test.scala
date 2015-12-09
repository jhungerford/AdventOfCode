package dev.adventofcode

import dev.adventofcode.Problem9.Edge
import org.scalatest.{Matchers, FlatSpec}

class Problem9Test extends FlatSpec with Matchers {
  behavior of "edge parser"

  it should "parse 'London to Dublin = 464' correctly" in {
    Problem9.parseEdge("London to Dublin = 464") shouldEqual Problem9.Edge("London", "Dublin", 464)
  }

  it should "parse 'London to Belfast = 518' correctly" in {
    Problem9.parseEdge("London to Belfast = 518") shouldEqual Problem9.Edge("London", "Belfast", 518)
  }

  it should "parse 'Dublin to Belfast = 141' correctly" in {
    Problem9.parseEdge("Dublin to Belfast = 141") shouldEqual Problem9.Edge("Dublin", "Belfast", 141)
  }

  behavior of "minimum spanning tree"

  it should "calculate the mst of london, belfast, and dublin" in {
    val edges = List[Edge] (
      Problem9.Edge("London", "Dublin", 464),
      Problem9.Edge("London", "Belfast", 518),
      Problem9.Edge("Dublin", "Belfast", 141)
    )

    val expectedMst = Set[Edge] (
      Problem9.Edge("London", "Dublin", 464),
      Problem9.Edge("Dublin", "Belfast", 141)
    )

    Problem9.minimumSpanningTree(edges) shouldEqual expectedMst
  }

  it should "Sum edges correctly" in {
    val edges = Set[Edge] (
      Problem9.Edge("London", "Dublin", 464),
      Problem9.Edge("Dublin", "Belfast", 141)
    )

    Problem9.sumEdges(edges) shouldEqual 605
  }
}
