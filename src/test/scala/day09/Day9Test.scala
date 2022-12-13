package com.owengilfellon
package day09

import helpers.BaseSpec

import com.owengilfellon.day09.Day9.{Down, Left, Point, Right, Up, parse}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class Day9Test extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """R 4
                                     |U 4
                                     |L 3
                                     |D 1
                                     |R 4
                                     |D 1
                                     |L 5
                                     |R 2"""

  "Day9" - {

    "parseCommand should return correct sequence of commands" in {
      Day9.parse(inputSeq.head) mustBe Seq(Right, Right, Right, Right)
      Day9.parse(inputSeq(1)) mustBe Seq(Up, Up, Up, Up)
      Day9.parse(inputSeq(2)) mustBe Seq(Left, Left, Left)
      Day9.parse(inputSeq(3)) mustBe Seq(Down)
    }

    def runXSteps(steps: Int): Seq[Point] = {
      inputSeq.take(steps).flatMap(parse).foldLeft(Seq(Point(0, 0), Point(0, 0)))((rope, move) => Day9.update(rope, move))
    }

    "update should be correct after" - {
      "one move" in {
        val x: Seq[Point] = runXSteps(1)
        x.head mustBe Point(4, 0)
        x.last mustBe Point(3, 0)
      }

      "two moves" in {
        val x: Seq[Point] = runXSteps(2)
        x.head mustBe Point(4, -4)
        x.last mustBe Point(4, -3)
      }

      "three moves" in {
        val x: Seq[Point] = runXSteps(3)
        x.head mustBe Point(1, -4)
        x.last mustBe Point(2, -4)
      }

      "four moves" in {
        val x: Seq[Point] = runXSteps(4)
        x.head mustBe Point(1, -3)
        x.last mustBe Point(2, -4)
      }

      "five moves" in {
        val x: Seq[Point] = runXSteps(5)
        x.head mustBe Point(5, -3)
        x.last mustBe Point(4, -3)
      }
    }

    "update and collect tail positions must collect correct tail positions" - {

      val head = Point(0, 0)
      val tail = Point(0, 0)

      "for first move" in {
        val x: Set[Point] = Day9.updateAllCollectTail(Seq(head, tail), parse(inputSeq.head))
        x mustBe Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0))
      }

      "for all move" in {
        val x: Set[Point] = Day9.updateAllCollectTail(Seq(head, tail), inputSeq.flatMap(parse))
        x.size mustBe 13
      }

      "for all moves with rope of length 10" in {
        val x: Set[Point] = Day9.updateAllCollectTail(Seq.fill(10)(Point(0,0)), inputSeq.flatMap(parse))
        x.size mustBe 1
      }
    }
  }
}
