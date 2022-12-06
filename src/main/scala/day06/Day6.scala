package com.owengilfellon
package day06

import helpers.FileInput

object Day6 extends FileInput {

  def startingPacket(packetSize: Int)(msg: String): Int =
    msg.sliding(packetSize).zipWithIndex
      .find(_._1.toSet.size == packetSize)
      .map(_._2 + packetSize) match {
      case Some(i) => i
      case _ => throw new NullPointerException("No starting packet")
    }

  def main(args: Array[String]): Unit = {
    println(startingPacket(4)(getLines("day06.txt").head))
    println(startingPacket(14)(getLines("day06.txt").head))
  }
}
