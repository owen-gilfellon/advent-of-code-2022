package com.owengilfellon
package day09

import helpers.FileInput

import scala.annotation.tailrec

object Day9 extends FileInput {

  sealed trait Move

  case object Up extends Move
  case object Down extends Move
  case object Left extends Move
  case object Right extends Move

  case class Point(x: Int, y: Int) {
    def -(point: Point): Point = Point(x - point.x, y - point.y)
    def +(point: Point): Point = Point(x + point.x, y + point.y)
  }

  def parse(line: String): Seq[Move] = {
    val el = line.split(" ")
    Seq.fill(el(1).toInt)(el.head.head match {
      case 'U' => Up
      case 'D' => Down
      case 'L' => Left
      case 'R' => Right
    })
  }

  def update(points: Seq[Point], move: Move): Seq[Point] = {

    def moveHead(point: Point, move: Move): Point = {
      move match {
        case Up => point.copy( y = point.y - 1 )
        case Down => point.copy( y = point.y + 1 )
        case Left => point.copy( x = point.x - 1 )
        case Right => point.copy( x = point.x + 1 )
      }
    }

    @tailrec
    def updateTail(updated: Seq[Point], original: Seq[Point]): Seq[Point] = {

      def moveTail(head: Point, tail: Point): Point = {
        (head - tail) match {

          case (Point(x, y)) if (Math.abs(x) <= 1 && Math.abs(y) <= 1) => tail

          case (Point(x, y)) if (x == 0 && y == 2) => tail.copy(y =  tail.y + 1)
          case (Point(x, y)) if (x == 0 && y == -2) => tail.copy(y =  tail.y - 1)
          case (Point(x, y)) if (y == 0 && x == 2) => tail.copy(x =  tail.x + 1)
          case (Point(x, y)) if (y == 0 && x == -2) => tail.copy(x =  tail.x - 1)


          case (Point(x, y)) if (x > 0 && y > 0) => tail.copy(x = tail.x + 1, y = tail.y + 1)
          case (Point(x, y)) if (x > 0 && y < 0) => tail.copy(x = tail.x + 1, y = tail.y -1)
          case (Point(x, y)) if (x < 0 && y < 0) => tail.copy(x = tail.x - 1, y = tail.y - 1)
          case (Point(x, y)) if (x < 0 && y > 0) => tail.copy(x = tail.x - 1, y = tail.y + 1)
          case _ => tail
        }
      }

      if(original.isEmpty)
        updated
      else
        updateTail(updated :+ moveTail(updated.last, original.head), original.tail)
    }

    updateTail(Seq(moveHead(points.head, move)), points.tail)
  }

  def update(points: Seq[Point], moves: Seq[Move]): Seq[Point] = {
    moves.foldLeft(points)(Day9.update)
  }

  def updateAllCollectTail(points: Seq[Point], moves: Seq[Move]): Set[Point] = {

    @tailrec
    def stepAndCollect(points: Seq[Point], moves: Seq[Move], tailHistory: Set[Point]): Set[Point] = {
      if(moves.isEmpty)
        tailHistory
      else {
        val updated = update(points, moves.head)
        stepAndCollect(updated, moves.tail, tailHistory + updated.last)
      }
    }

    stepAndCollect(points, moves, Set())
  }

  def main(args: Array[String]): Unit = {
    val head = Point(0, 0)
    val tail = Point(0, 0)
    val moves = getLines("day09.txt").flatMap(parse)
    println(s"Part 1: ${updateAllCollectTail(Seq(head, tail), moves).size}")
    println(s"Part 2: ${updateAllCollectTail(Seq.fill(10)(Point(0,0)), moves).size}")
  }

}
