package dev.adventofcode.problem5

import java.io.File

import com.google.common.io.Resources

import scala.collection.immutable.IndexedSeq
import scala.io.Source

object Problem5 {

  sealed trait NiceRules {
    def isNice(str: String): Boolean
  }

  object Part1Rules extends NiceRules {
    /**
      * A nice String meets all of the following rules:
      * It contains at least three vowels (aeiou)
      * It contains at least one letter that appears twice in a row
      * It does not contain the strings ab, cd, pq, or xy
      *
      * @param str String to check
      * @return Whether the string is nice
      */
    def isNice(str: String): Boolean = {
      val pairs = pairLetters(str)

      hasEnoughVowels(str) &&
        !hasAForbiddenString(pairs) &&
        hasALetterTwiceInARow(pairs)
    }

    val vowels = Set('a', 'e', 'i', 'o', 'u')

    def hasEnoughVowels(str: String): Boolean = {
      str.filter(letter => vowels.contains(letter)).length >= 3
    }

    val forbidden = Set(
      ('a', 'b'),
      ('c', 'd'),
      ('p', 'q'),
      ('x', 'y')
    )

    def hasAForbiddenString(pairs: List[(Char, Char)]) = pairs.exists(pair => forbidden.contains(pair))

    def hasALetterTwiceInARow(pairs: List[(Char, Char)]) = pairs.exists(pair => pair._1 == pair._2)
  }

  def pairLetters(str: String): List[(Char, Char)] = {
    str.tail.foldLeft((str.head, List.empty[(Char, Char)])) { case ((prevLetter, pairs), letter) =>
      (letter, pairs :+(prevLetter, letter))
    }._2
  }

  def countNice(iterator: Iterator[String], rules: NiceRules): Int = iterator.count(word => rules.isNice(word))

  def main(args: Array[String]) {
    val inputFile = new File(Resources.getResource("problem5/input").toURI)

    System.out.println(s"Nice words: ${countNice(Source.fromFile(inputFile).getLines(), Part1Rules)}")
  }
}
