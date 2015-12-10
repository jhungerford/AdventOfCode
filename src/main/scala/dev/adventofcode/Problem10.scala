package dev.adventofcode

object Problem10 {

  case class LookAndSayState(sayString: String, prevChar: Char, prevCount: Int)

  def lookAndSay(str: String): String = {
    def say(state: LookAndSayState): String = s"${state.sayString}${state.prevCount}${state.prevChar}"

    val finalState: LookAndSayState = str.tail.foldLeft(LookAndSayState("", str.head, 1)) { (state, char) =>
      char == state.prevChar match {
        case true => state.copy(prevCount = state.prevCount + 1)
        case false => state.copy(
          sayString = say(state),
          prevChar = char,
          prevCount = 1
        )
      }
    }

    say(finalState)
  }

  def main(args: Array[String]) {
    val input = "1113122113"

    val result = (1 to 40).foldLeft(input) { (str, i) =>
      val result = lookAndSay(str)
      System.out.println(s"Iteration $i length: ${result.length}")
      result
    }

    System.out.println(s"Result of result after 40 times: ${result.length}")
  }
}
