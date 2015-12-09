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

  // https://en.wikipedia.org/wiki/Kruskal%27s_algorithm
  it should "calculate the mst of the wikipedia example correclty" in {
    val edges = List[Edge] (
      Problem9.Edge("a", "b", 7),
      Problem9.Edge("b", "c", 8),
      Problem9.Edge("a", "d", 5),
      Problem9.Edge("d", "b", 9),
      Problem9.Edge("b", "e", 7),
      Problem9.Edge("e", "c", 5),
      Problem9.Edge("d", "e", 15),
      Problem9.Edge("d", "f", 6),
      Problem9.Edge("f", "g", 11),
      Problem9.Edge("f", "e", 8),
      Problem9.Edge("e", "g", 9)
    )

    val expectedMst = Set[Edge] (
      Problem9.Edge("a", "b", 7),
      Problem9.Edge("b", "e", 7),
      Problem9.Edge("e", "c", 5),
      Problem9.Edge("e", "g", 9),
      Problem9.Edge("a", "d", 5),
      Problem9.Edge("d", "f", 6)
    )

    Problem9.minimumSpanningTree(edges) shouldEqual expectedMst
  }

  it should "detect that an edge causes a cycle" in {
    val tree = Set[Edge] (
      Problem9.Edge("London", "Dublin", 464),
      Problem9.Edge("Dublin", "Belfast", 141)
    )

    Problem9.edgeCausesCycle(tree, new Edge("Dublin", "London", 1)) shouldEqual true
    Problem9.edgeCausesCycle(tree, new Edge("Belfast", "London", 1)) shouldEqual true
    Problem9.edgeCausesCycle(tree, new Edge("Dublin", "Belfast", 1)) shouldEqual true
    Problem9.edgeCausesCycle(tree, new Edge("Dublin", "City that isn't in the tree", 1)) shouldEqual false
  }

  behavior of "sum edges"

  it should "sum edges correctly" in {
    val edges = Set[Edge] (
      Problem9.Edge("London", "Dublin", 464),
      Problem9.Edge("Dublin", "Belfast", 141)
    )

    Problem9.sumEdges(edges) shouldEqual 605
  }
}
