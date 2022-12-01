package com.owengilfellon
package day01

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class CalorieCounterTest extends AnyFreeSpec {

  val testInput: Seq[String] =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin.split("\n").toSeq

  "stringsToOptInt" - {

    val opts = CalorieCounter.stringsToOptInt(testInput)

    "must parse all lines" in {
      opts.size mustBe 14
    }

    "must return all numbers" in {
      opts.count(_.isDefined) mustBe 10
    }

    "must return all empty lines as none" in {
      opts.count(_.isEmpty) mustBe 4
    }

    "must work with empty input" in {
      CalorieCounter.stringsToOptInt(Seq()).size mustBe 0
    }

    "must work with head empty line" in {
      val input:Seq[String] = testInput ++: Seq("")
      CalorieCounter.stringsToOptInt(input).size mustBe 15
    }

    "must work with trailing empty line" in {
      val input:Seq[String] = testInput :++ Seq("")
      CalorieCounter.stringsToOptInt(input).size mustBe 15
    }
  }

  "groupNumbers" - {

    val opts = CalorieCounter.stringsToOptInt(testInput)
    val grouped = CalorieCounter.groupNumbers(opts)

    "must group numbers" in {
      grouped.size mustBe 5
    }

    "must return all numbers in groups" in {
      grouped.flatten.size mustBe 10
    }

    "must work with empty input" in {
      val opts = CalorieCounter.stringsToOptInt(Seq())
      val grouped = CalorieCounter.groupNumbers(opts)
      grouped.size mustBe 0
    }

    "must work with head empty line" in {
      val input:Seq[String] = testInput ++: Seq("")
      val opts = CalorieCounter.stringsToOptInt(input)
      val grouped = CalorieCounter.groupNumbers(opts)
      grouped.size mustBe 5
    }

    "must work with trailing empty line" in {
      val input:Seq[String] = testInput :++ Seq("")
      val opts = CalorieCounter.stringsToOptInt(input)
      val grouped = CalorieCounter.groupNumbers(opts)
      grouped.size mustBe 5
    }
  }

  "getCounts" - {
    val opts = CalorieCounter.stringsToOptInt(testInput)
    val grouped = CalorieCounter.groupNumbers(opts)
    val counts = CalorieCounter.getCounts(grouped)

    "must sum groups" in {
      counts.size mustBe 5
      counts.head mustBe 6000
      counts(1) mustBe 4000
      counts(2) mustBe 11000
      counts(3) mustBe 24000
      counts(4) mustBe 10000
    }
  }
}
