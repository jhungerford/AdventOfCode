package dev.adventofcode

import java.io.File

import com.google.common.io.Resources

import scala.io.Source

object Problem9 {

  case class Edge(city1: String, city2: String, distance: Int)

  def parseEdge(line: String): Edge = ???

  def minimumSpanningTree(edges: List[Edge]): Set[Edge] = ???

  def sumEdges(edges: Set[Edge]): Int = ???

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem8/input").toURI)
    val edges = Source.fromFile(inputFile).getLines().map(parseEdge).toList

    val mst = minimumSpanningTree(edges)
    System.out.println(s"Shortest route: ${sumEdges(mst)}")
  }
}
