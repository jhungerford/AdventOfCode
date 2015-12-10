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

  def main(args: Array[String]): Unit = {
    val input = "1113122113"

    // https://www.youtube.com/watch?v=ea7lJkEhytA - Conway's Constant governs the growth of digits
    // This block applies conway's constant and runs quickly, but produces the wrong result
    /*
    val resultAfter35 = (1 to 35).foldLeft(input) { (str, i) =>
      lookAndSay(str)
    }

    val conwaysConstant = 1.303577269
    val productAfter50 = (36 to 50).foldLeft(resultAfter35.length.toDouble) { (length, i) =>
      val product = length * conwaysConstant
      System.out.println(s"Product at $i: $product")
      product
    }
    System.out.println(s"Length after 50 by applying conways constant: $productAfter50")
    */

    // This approach gets the right answer, but takes several hours to run.
    val result = (1 to 40).foldLeft(input) { (str, i) =>
      val start = System.currentTimeMillis()
      val result = lookAndSay(str)
      val stop = System.currentTimeMillis()
      System.out.println(s"Iteration $i length: ${result.length} - took ${stop-start} ms")
      result
    }

    System.out.println(s"Result of result after 40 times: ${result.length}")

    val result50 = (41 to 50).foldLeft(result) { (str, i) =>
      val start = System.currentTimeMillis()
      val result = lookAndSay(str)
      val stop = System.currentTimeMillis()
      System.out.println(s"Iteration $i length: ${result.length} - took ${stop-start} ms")
      result
    }

    System.out.println(s"Result of result after 50 iterations: ${result50.length}")
  }
}
