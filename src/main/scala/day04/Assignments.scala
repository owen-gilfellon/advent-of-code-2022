package com.owengilfellon
package day04

import helpers.FileInput

object Assignments extends FileInput {

  case class Assignment(from: Int, to: Int)

  def assignmentPair(line: String): (Assignment, Assignment) = {
    val linePattern = "^([\\d]+)-([\\d]+),([\\d]+)-([\\d]+)$".r
    linePattern.findFirstMatchIn(line) match {
      case Some(m) =>  (Assignment(m.group(1).toInt, m.group(2).toInt), Assignment(m.group(3).toInt, m.group(4).toInt))
      case _ => throw new IllegalArgumentException(s"Line does not match pattern - $line")
    }
  }

  def containsAll(pair: (Assignment, Assignment)): Boolean = {
    (pair._1.from >= pair._2.from && pair._1.to <= pair._2.to) ||
      (pair._2.from >= pair._1.from && pair._2.to <= pair._1.to)
  }

  def containsPartial(pair: (Assignment, Assignment)): Boolean = {
    (pair._1.from to pair._1.to)
      .intersect((pair._2.from to pair._2.to)).nonEmpty
  }

  def main(args: Array[String]): Unit = {
    val lines = getLines("day04.txt")
    println(s"Part 1: ${lines.map(Assignments.assignmentPair).count(Assignments.containsAll)}")
    println(s"Part 2: ${lines.map(Assignments.assignmentPair).count(Assignments.containsPartial)}")
  }
}
