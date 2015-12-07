package dev.adventofcode

import java.security.MessageDigest

import org.apache.commons.codec.binary.Hex

object Problem4 {

  def firstHashWithFiveZeroes = firstHashWithNZeros(5) _
  def firstHashWithSixZeroes = firstHashWithNZeros(6) _

  def firstHashWithNZeros(zeros: Int)(key: String) = {
    val startsWith = new String((1 to zeros).map(_ => '0').toArray)

    Stream.from(1)
      .map(i => (i, s"$key$i"))
      .map{
        case (i, input) => (i, md5(input))
      }.collectFirst{
        case (i, hash) if hash.startsWith(startsWith) => i
      }.get
  }

  def md5(str: String): String = Hex.encodeHexString(MessageDigest.getInstance("MD5").digest(str.getBytes))

  def main(args: Array[String]) {
    val key = "ckczppom"

    System.out.println(s"First hash with 5 zeroes for key $key: ${firstHashWithFiveZeroes(key)}")
    System.out.println(s"First hash with 6 zeroes for key $key: ${firstHashWithSixZeroes(key)}")
  }
}
