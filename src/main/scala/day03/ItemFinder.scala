package com.owengilfellon
package day03

import helpers.FileInput

import scala.annotation.tailrec

object ItemFinder extends FileInput {

  def findBadge(members: Seq[String]): Char = {

    @tailrec
    def find(member: String, members: Seq[String]): Seq[Char] = {
      if(members.isEmpty)
        member else
      find(members.head.filter(member.contains(_)), members.tail)
    }

    find(members.head, members.tail).headOption match {
      case Some(badge) => badge
      case _ => throw new IllegalArgumentException("No badge in input")
    }
  }

  def find(items: String): Char = {
    val compartments: (String, String) = items.splitAt(items.size / 2)
    assert(compartments._1.length == compartments._2.length)
    compartments._1.find(compartments._2.contains(_)) match {
      case Some(commonItem) => commonItem
      case _ => throw new IllegalArgumentException("No item in input")
    }
  }

  private val priorities: Seq[(Char, Int)] =
    ('a' to 'z').zip(1 to 26) :++ ('A' to 'Z').zip(27 to 52)

  def priority(char: Char): Int = {
    priorities.find(_._1 == char) match {
      case Some(priority) => priority._2
      case _ => throw new IllegalArgumentException("Item has no priority")
    }
  }

  def sumPriorities(compartments: Seq[String]): BigInt = {
    compartments.map(find).map(priority).map(BigInt.int2bigInt).sum
  }

  def sumBadgePriorities(compartments: Seq[String]): BigInt = {
    compartments.grouped(3).map(findBadge).map(priority).map(BigInt.int2bigInt).sum
  }

  def main(args: Array[String]): Unit = {
    println(sumPriorities(getLines("day03.txt")))
    println(sumBadgePriorities(getLines("day03.txt")))
  }

}
