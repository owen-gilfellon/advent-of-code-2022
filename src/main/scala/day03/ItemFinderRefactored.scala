package com.owengilfellon
package day03

import helpers.FileInput

import scala.annotation.tailrec

object ItemFinderRefactored extends FileInput {

  def find(groupSize:Int)(compartments: Seq[String]): BigInt = {

    def findCommonInAll(compartments: Seq[String]): Char = {

      // recursively reduce the set of possible common items
      @tailrec
      def find(item: String, items: Seq[String]): Seq[Char] = {
        if(items.isEmpty)
          item
        else
          find(items.head.filter(item.contains(_)), items.tail)
      }

      find(compartments.head, compartments.tail).headOption match {
        case Some(item) => item
        case _ => throw new IllegalArgumentException("No common item in input")
      }
    }

    compartments.grouped(groupSize).map(findCommonInAll).map(prioritise).sum
  }

  private val priorities: Seq[(Char, Int)] =
    ('a' to 'z').zip(1 to 26) :++ ('A' to 'Z').zip(27 to 52)

  def prioritise(char: Char): Int = {
    priorities.find(_._1 == char) match {
      case Some(priority) => priority._2
      case _ => throw new IllegalArgumentException("Item has no priority")
    }
  }

  def split(string: String): Seq[String] = {
    val s = string.splitAt(string.length / 2)
    Seq(s._1, s._2)
  }

  def main(args: Array[String]): Unit = {

    println(find(2)(getLines("day03.txt").flatMap(split)))
    println(find(3)(getLines("day03.txt")))
  }
}
