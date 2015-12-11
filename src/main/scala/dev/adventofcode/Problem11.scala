package dev.adventofcode

object Problem11 {

  def hasStraight(str: String): Boolean = {
    (2 until str.length).exists { i =>
      str(i-2) == str(i-1) - 1 && str(i-1) == str(i) - 1
    }
  }

  def noIOL(str: String): Boolean = ! (str.contains('i') || str.contains('o') || str.contains('l'))

  def hasPairs(str: String): Boolean = {
    val pairs: List[(Char, Char)] = Problem5.pairLetters(str).filter(pair => pair._1 == pair._2 )
    pairs.toSet.size >= 2
  }

  def increment(str: String): String = {
    str.last match {
      case 'z' => increment(str.init) + 'a'
      case letter => str.init + (letter + 1).toChar
    }
  }

  def main(args: Array[String]) {
    val password = "vzbxkghb"

    val passwords: Stream[String] = Stream.iterate(password)(increment)
    val newPasswords = passwords.collect{
      case pass if hasStraight(pass) && noIOL(pass) && hasPairs(pass) => pass
    }.take(2).toList

    System.out.println(s"New passwords: $newPasswords")
  }
}
