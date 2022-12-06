package com.owengilfellon
package day06

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class Day6Test extends AnyFreeSpec  {

  "Day6" - {

    "startingPacket with length 4 must provide correct value for" - {

      val startingPacket_4char = Day6.startingPacket(4)(_)

      "example 1" in {
        val input1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        startingPacket_4char(input1) mustBe 7
      }
      "example 2" in {
        val input2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
        startingPacket_4char(input2) mustBe 5
      }
      "example 3" in {
        val input3 = "nppdvjthqldpwncqszvftbrmjlhg"
        startingPacket_4char(input3) mustBe 6
      }
      "example 4" in {
        val input4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        startingPacket_4char(input4) mustBe 10
      }
      "example 5" in {
        val input5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        startingPacket_4char(input5) mustBe 11
      }
    }

    "startingPacket with length 14 must provide correct value for" - {

      val startingPacket_4char = Day6.startingPacket(14)(_)

      "example 1" in {
        val input1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        startingPacket_4char(input1) mustBe 19
      }
      "example 2" in {
        val input2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
        startingPacket_4char(input2) mustBe 23
      }
      "example 3" in {
        val input3 = "nppdvjthqldpwncqszvftbrmjlhg"
        startingPacket_4char(input3) mustBe 23
      }
      "example 4" in {
        val input4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        startingPacket_4char(input4) mustBe 29
      }
      "example 5" in {
        val input5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        startingPacket_4char(input5) mustBe 26
      }
    }
  }
}
