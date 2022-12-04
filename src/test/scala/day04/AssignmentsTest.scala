package com.owengilfellon
package day04

import day04.Assignments.Assignment
import helpers.BaseSpec

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class AssignmentsTest extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """2-4,6-8
                                     |2-3,4-5
                                     |5-7,7-9
                                     |2-8,3-7
                                     |6-6,4-6
                                     |2-6,4-8""".stripMargin

  "assignmentPair should parse lines correctly" in {
    val lines = inputSeq
    Assignments.assignmentPair(lines.head) mustBe (Assignment(2, 4), Assignment(6, 8))
    Assignments.assignmentPair(lines(1)) mustBe (Assignment(2, 3), Assignment(4, 5))
    Assignments.assignmentPair(lines(2)) mustBe (Assignment(5, 7), Assignment(7, 9))
    Assignments.assignmentPair(lines(3)) mustBe (Assignment(2, 8), Assignment(3, 7))
    Assignments.assignmentPair(lines(4)) mustBe (Assignment(6, 6), Assignment(4, 6))
    Assignments.assignmentPair(lines(5)) mustBe (Assignment(2, 6), Assignment(4, 8))
  }

  "containsAll should return correct pairs" in {
    val lines = inputSeq
    val containing = lines.map(Assignments.assignmentPair).filter(Assignments.containsAll)
    containing.length mustBe 2
    containing mustBe Seq(
      (Assignment(2, 8), Assignment(3, 7)),
      (Assignment(6, 6), Assignment(4, 6)))
  }

  "containsPartial should return correct pairs" in {
    val lines = inputSeq
    val containing = lines.map(Assignments.assignmentPair).filter(Assignments.containsPartial)
    containing.length mustBe 4
  }
}
