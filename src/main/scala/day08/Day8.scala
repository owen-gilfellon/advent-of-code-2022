package com.owengilfellon
package day08

import helpers.FileInput

object Day8 extends FileInput {

  def largestFromLeft(index: Int, seq: Seq[Int]): Boolean = {
    seq.takeWhile(i => i < seq(index)).size >= index
  }

  def largestFromEither(index: Int, seq: Seq[Int]): Boolean = {
    largestFromLeft(index, seq) || largestFromLeft(seq.size - 1 - index, seq.reverse)
  }

  def largestFromAny(x: Int, y: Int, table: Seq[Seq[Int]]): Boolean = {
   largestFromEither(x, table(y)) || largestFromEither(y, transpose(table)(x))
  }

  def largestFromAnyEdge(table: Seq[Seq[Int]]): Seq[Seq[Boolean]] = {
    table.indices.map(y => {
      table(y).indices.map(x => {
        largestFromAny(x, y, table)
      })
    })
  }

  def scenicScoreToRight(index: Int, seq: Seq[Int]): Int = {
    if(index == seq.size - 1) 0 else
    (index + 1 until seq.size).find(i => seq(i) >= seq(index)).map(i => i - index).getOrElse(seq.size - index - 1)
  }

  def scenicScoreToLeft(index: Int, seq: Seq[Int]): Int = {
   scenicScoreToRight(seq.size - 1 - index, seq.reverse)
  }

  def scenicScore(x: Int, y: Int, table: Seq[Seq[Int]]): Int = {
    (x, y) match {
      case _ => {
        val transposed = transpose(table)
        scenicScoreToLeft(x, table(y)) * scenicScoreToRight(x, table(y)) *
          scenicScoreToLeft(y, transposed(x)) * scenicScoreToRight(y, transposed(x))}
    }
  }

  def scenicScores(table: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    table.indices.map(y => {
      table(y).indices.map(x => {
        scenicScore(x, y, table)
      })
    })
  }

  def transpose[A](table: Seq[Seq[A]]): Seq[Seq[A]] =
    (0 to table.head.size).map(i => table.flatMap(_.lift(i)))

  def main(args: Array[String]): Unit = {
    val table = getLines("day08.txt").map(_.toCharArray.map(_.toString.toInt).toSeq)
    println(s"Part 1: ${largestFromAnyEdge(table).flatten.count(_ == true)}")
    println(s"Part 2: ${scenicScores(table).flatten.max}")
  }
}
