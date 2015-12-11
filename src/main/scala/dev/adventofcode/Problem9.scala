package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.collection.mutable
import scala.io.Source

object Problem9 {

  case class Edge(city1: String, city2: String, distance: Int)

  val edgeRegex = """(\w+) to (\w+) = (\d+)""".r
  def parseEdge(line: String): Edge = line match {
    case edgeRegex(city1, city2, distanceStr) => Edge(city1, city2, distanceStr.toInt)
  }

  def edgeCausesCycle(tree: Set[Edge], edge: Edge): Boolean = {
    def makeCitySet(city: String, nodes: Set[Edge]): Set[String] = {
      val maybeCityEdge = nodes.find { e => e.city1 == city || e.city2 == city }

      maybeCityEdge match {
        case Some(cityEdge) =>
          val nodesWithoutCityEdge = nodes - cityEdge
          makeCitySet(cityEdge.city1, nodesWithoutCityEdge) ++ makeCitySet(cityEdge.city2, nodesWithoutCityEdge)

        case None => Set(city)
      }
    }

    makeCitySet(edge.city1, tree) == makeCitySet(edge.city2, tree)
  }

  def minimumSpanningTree(edges: List[Edge]): Set[Edge] = {
    // kruskal's algorithm.  Sort edges by distance, keep adding ones that don't make a cycle until the tree is full
    val sortedEdges = edges.sortBy(_.distance)

    sortedEdges.foldLeft(Set.empty[Edge]) { (tree, edge) =>
      edgeCausesCycle(tree, edge) match {
        case true => tree
        case false => tree + edge
      }
    }
  }

  def sumEdges(edges: Set[Edge]): Int = edges.map(_.distance).sum

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem9/input").toURI)
    val edges = Source.fromFile(inputFile).getLines().map(parseEdge).toList

    val citiesToDistance = edges.map(edge => Set(edge.city1, edge.city2) -> edge.distance).toMap

    // Problem 9 is the traveling salesman problem minus one edge.
    // There's only 7 cities - brute force the solution.
    val cities = edges.flatMap(edge => List(edge.city1, edge.city2)).toSet
    val pathLengths = cities.toList.permutations.map { (edges) =>
      val pathDistances: List[Int] = edges.tail.foldLeft((edges.head, List.empty[Int])) { case ((prevCity, distances), city) =>
        val distance = citiesToDistance(Set(prevCity, city))
        (city, distances :+ distance)
      }._2

      pathDistances.sum
    }.toList.sorted

    System.out.println(s"Shortest path: ${pathLengths.head}")
    System.out.println(s"Longest path: ${pathLengths.last}")
  }
}
