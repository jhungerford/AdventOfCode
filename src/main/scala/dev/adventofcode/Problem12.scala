package dev.adventofcode

import java.io.File

import com.fasterxml.jackson.databind.node.{NumericNode, TextNode, ArrayNode, ObjectNode}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.google.common.io.Resources

import scala.io.Source

import scala.collection.JavaConversions._

object Problem12 {

  def isRedObject(obj: ObjectNode): Boolean = {
    obj.elements.toIterator.exists {
      case t: TextNode if t.textValue() == "red" => true
      case _ => false
    }
  }

  def sumNumbers(jsonNode: JsonNode): Int = jsonNode match {
    case arr: ArrayNode =>
      arr.toIterable.map { child => sumNumbers(child) }.sum
    case obj: ObjectNode =>
      isRedObject(obj) match {
        case true => 0
        case false => obj.elements.map { element => sumNumbers(element) }.sum
      }
    case num: NumericNode => num.asInt()
    case _ => 0
  }

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem12/input").toURI)
    val jsonStr = Source.fromFile(inputFile).getLines().next()

    val numberRegex = """(-?[0-9]+)""".r
    val sum = numberRegex.findAllMatchIn(jsonStr)
      .map{numberMatch => numberMatch.group(1).toInt}
      .sum

    System.out.println(s"Sum of all numbers in the document: $sum")

    val json: JsonNode = new ObjectMapper().readTree(jsonStr)
    System.out.println(s"Sum ignoring reds: ${sumNumbers(json)}")
  }
}
