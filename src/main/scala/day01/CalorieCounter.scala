package com.owengilfellon
package day01

import helpers.FileInput

import scala.annotation.tailrec

object CalorieCounter extends FileInput {

  def stringsToOptInt(lines: Seq[String]): Seq[Option[Int]] = {
    lines.map {
      case s if s.trim.nonEmpty  => Some(s.toInt)
      case _ => None
    }
  }

  def groupNumbers(lines: Seq[Option[Int]]): Seq[Seq[Int]] = {

    @tailrec
    def groupNumbersRec(lines: Seq[Option[Int]], grouped: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      if(lines.flatten.isEmpty)
        grouped
      else {
        val currentGroup: Seq[Int] = lines.dropWhile(_.isEmpty).takeWhile(_.nonEmpty).flatten
        val remainingInput: Seq[Option[Int]] = lines.dropWhile(_.isEmpty).dropWhile(_.nonEmpty)
        val collectedGroups: Seq[Seq[Int]] = grouped :+ currentGroup
        groupNumbersRec(remainingInput, collectedGroups)
      }
    }

    groupNumbersRec(lines, Seq())
  }

  def getCounts(numberGroups: Seq[Seq[Int]]): Seq[Int] = {
    numberGroups.map(_.sum)
  }

  def main(args: Array[String]): Unit = {
    val lines = getLines("day01.txt")
    val linesAsOptionalInts = stringsToOptInt(lines)
    val groupedInts = groupNumbers(linesAsOptionalInts)
    val summedGroups = getCounts(numberGroups = groupedInts)

    println(s"Highest count: ${summedGroups.max}")
    println(s"Highest 3 counts: ${summedGroups.sorted.takeRight(3).sum}")
  }
}
