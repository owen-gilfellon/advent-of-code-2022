package com.owengilfellon
package day08

import helpers.BaseSpec

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class Day8Test extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """30373
                                     |25512
                                     |65332
                                     |33549
                                     |35390"""
  "Day8" - {

    "transpose should transpose" in {
      val transposed = Day8.transpose(inputSeq.map(_.map(_.toString).map(_.toInt)))
      transposed.head mustBe Seq(3, 2, 6, 3, 3)
      transposed(1) mustBe Seq(0, 5, 5, 3, 5)
    }

    "largestFromLeft" in {
      val line1 = inputSeq.head.map(_.toString).map(_.toInt)
      val line1VisibleFromLeft = line1.indices.map(i => Day8.largestFromLeft(i, line1))
      line1VisibleFromLeft mustBe Seq(true, false, false, true, false)

      val line2 = inputSeq(1).map(_.toString).map(_.toInt)
      val line2VisibleFromLeft = line2.indices.map(i => Day8.largestFromLeft(i, line2))
      line2VisibleFromLeft mustBe Seq(true, true, false, false, false)
    }

    "largestFromEither" in {
      val line1 = inputSeq.head.map(_.toString).map(_.toInt)
      val line1VisibleFromLeft = line1.indices.map(i => Day8.largestFromEither(i, line1))
      line1VisibleFromLeft mustBe Seq(true, false, false, true, true)

      val line2 = inputSeq(1).map(_.toString).map(_.toInt)
      val line2VisibleFromLeft = line2.indices.map(i => Day8.largestFromEither(i, line2))
      line2VisibleFromLeft mustBe Seq(true, true, true, false, true)
    }

    "largestFromAny" in {
      val table = inputSeq.map(_.map(_.toString).map(_.toInt))
      val visibility = Day8.largestFromAnyEdge(table)
      visibility.head mustBe Seq(true, true, true, true, true)
      visibility(1) mustBe Seq(true, true, true, false, true)
    }


    "scenic score  from left" in {
      val table = inputSeq.map(_.map(_.toString).map(_.toInt))
      val visibility = table.head.indices.map(i => Day8.scenicScoreToLeft(i, table.head))
      val visibility2 = table(1).indices.map(i => Day8.scenicScoreToLeft(i, table(1)))
      visibility mustBe Seq(0, 1, 2, 3, 1)
      visibility2 mustBe Seq(0, 1, 1, 1, 2)
    }

    "scenic score  from right" in {
      val table = inputSeq.map(_.map(_.toString).map(_.toInt))
      val visibility = table.head.indices.map(i => Day8.scenicScoreToRight(i, table.head))
      val visibility2 = table(1).indices.map(i => Day8.scenicScoreToRight(i, table(1)))
      visibility mustBe Seq(2, 1, 1, 1, 0)
      visibility2 mustBe Seq(1, 1, 2, 1, 0)
    }

    "Scores" in {
      val table = inputSeq.map(_.map(_.toString).map(_.toInt))
      val visibility = Day8.scenicScores(table)
      visibility.flatten.max mustBe 8
    }
  }
}
