package dev.adventofcode

import org.scalatest.{FlatSpec, Matchers}

class Problem4Test extends FlatSpec with Matchers {

  behavior of "md5"

  it should "hash abcdef609043 correctly" in {
    Problem4.md5("abcdef609043") should startWith("000001dbbfa")
  }

  it should "not start with 5 zeros for abcdef2197" in {
    Problem4.md5("abcdef2197") shouldNot startWith("00000")
  }

  it should "hash pqrstuv1048970 correctly" in {
    Problem4.md5("pqrstuv1048970") should startWith("000006136ef")
  }


  behavior of "first hash with 5 zeroes"

  it should "get 609043 for key abcdef" in {
    Problem4.firstHashWithFiveZeroes("abcdef") shouldBe 609043
  }

  it should "get 1048970 for key pqrstuv" in {
    Problem4.firstHashWithFiveZeroes("pqrstuv") shouldBe 1048970
  }
}
