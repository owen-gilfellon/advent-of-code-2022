package com.owengilfellon
package day05

import com.owengilfellon.helpers.FileInput

import scala.util.matching.Regex

object Stacker extends FileInput {

  def getStacks(columns: Int)(lines: Seq[String]): Seq[String] = {

    def parseLine(line: String): Seq[Option[Char]] =
      (0 until columns).map(_ * 4).map(_ + 1)
        .map(line.padTo(columns * 4, ' ').charAt)
        .map(Option.apply).map {
        case Some(' ') => None
        case Some(char) => Some(char)
      }

    // parse rows, including spaces
    val rows: Seq[Seq[Option[Char]]] = lines.map(parseLine)
    // transpose rows to get columns as strings
    (0 until columns).map(column =>  rows.flatMap(row => row(column))).map(_.mkString)
  }

  def getMoves(input: String): Seq[(Int, Int, Int)] = {
    val regex = """move ([\d]+) from ([\d]+) to ([\d]+)""".r
    regex.findAllMatchIn(input)
      .map { m: Regex.Match => (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt) }
      .toSeq
  }

  def applyMove(stacks: Seq[String], move: (Int, Int, Int), f: String => String = a => a): Seq[String] =
    stacks
      .updated(move._2 - 1, stacks(move._2 - 1).drop(move._1))
      .updated(move._3 - 1, f(stacks(move._2 - 1).take(move._1)) + stacks( move._3 - 1))



  def main(args: Array[String]): Unit = {

    val lines = getLines("day05.txt")

    val stacks = getStacks(9)(lines.take(8))
    val moves = getMoves(lines.mkString("\n"))

    def topBoxes(stacks: Seq[String]): String =
      stacks.flatMap(_.headOption).mkString

    val partOne = moves.foldLeft(stacks)((a, b) => applyMove(a, b, a => a.reverse))

    println( topBoxes(partOne) )

    val partTwo = moves.foldLeft(stacks)((a, b) => applyMove(a, b))

    println( topBoxes(partTwo) )
  }
}
