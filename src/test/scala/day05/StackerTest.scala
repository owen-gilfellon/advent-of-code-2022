package com.owengilfellon
package day05

import com.owengilfellon.helpers.BaseSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class StackerTest extends AnyFreeSpec with BaseSpec {
  override val testInput: String = """    [D]
                                     |[N] [C]
                                     |[Z] [M] [P]
                                     | 1   2   3
                                     |
                                     |move 1 from 2 to 1
                                     |move 3 from 1 to 3
                                     |move 2 from 2 to 1
                                     |move 1 from 1 to 2"""


  "Stacker" - {

    "getColumns must return sacked columns" in {
      val crates: Seq[String] = Stacker.getStacks(3)(inputSeq.take(3))
      crates.size mustBe 3
      crates.head mustBe "NZ"
      crates(1) mustBe "DCM"
      crates(2) mustBe "P"
      println(crates)
    }

    "getMoves must return moves" in {
      val moves: Seq[(Int, Int, Int)] = Stacker.getMoves(testInput.stripMargin)
      moves.head mustBe (1, 2, 1)
      moves(1) mustBe (3, 1, 3)
      moves(2) mustBe (2, 2, 1)
      moves(3) mustBe (1, 1, 2)
    }

    "applySingleBoxes" in {
      val crates: Seq[String] = Stacker.getStacks(3)(inputSeq.take(3))
      val moves: Seq[(Int, Int, Int)] = Stacker.getMoves(testInput.stripMargin)

      val updated = Stacker.applyMove(crates, moves.head)
      updated.head mustBe "DNZ"
      updated(1) mustBe "CM"
      updated(2) mustBe "P"

      val updated2 = Stacker.applyMove(updated, moves(1), a => a.reverse)
      updated2.head mustBe ""
      updated2(1) mustBe "CM"
      updated2(2) mustBe "ZNDP"
    }
  }
}
