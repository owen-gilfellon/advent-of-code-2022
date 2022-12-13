package com.owengilfellon
package day11

import helpers.FileInput

object Day11 extends FileInput {

  case class Monkey(id: Int, items: Seq[BigInt], op: BigInt => BigInt, test: BigInt => Int, inspected: Long = 0){
    override def toString: String = s"Monkey $id: ${items.mkString(", ")}"
  }

  def getDivisors(monkeyDefs: Seq[String]): Seq[Int] = {
    monkeyDefs.filter(_.trim.nonEmpty).grouped(6).map(monkeyDef => {
      monkeyDef(3).trim.split(" ")(3).toInt
    }).toSeq
  }

  def parse(monkeyDefs: Seq[String]): Seq[Monkey] = {

    val testRegex = "new = old ([*+]) ([\\d]+|old)".r
    val numberRegex = "([\\d]+)".r

    monkeyDefs.filter(_.trim.nonEmpty).grouped(6).map(monkeyDef => {
      val id = monkeyDef.head.trim.replace(":", "").split(" ")(1).toInt
      val startingItems = numberRegex.findAllMatchIn( monkeyDef(1)).map(_.group(1).toInt).map(BigInt.apply).toSeq
      val operationMatch = testRegex.findFirstMatchIn(monkeyDef(2))
      val operation = (operationMatch.get.group(1), operationMatch.get.group(2)) match {
        case(s1, s2) if s1 == "*" && s2 == "old" => (x:BigInt) => x * x
        case(s1, s2) if s1 == "+" && s2 == "old" => (x:BigInt) => x + x
        case(s1, s2) if s1 == "*" => (x:BigInt) => x * s2.toInt
        case(s1, s2) if s1 == "+" => (x:BigInt) => x + s2.toInt
      }
      val divisor = monkeyDef(3).trim.split(" ")(3).toInt
      val nextMonkeyTrue = monkeyDef(4).trim.split(" ")(5).toInt
      val nextMonkeyFalse = monkeyDef(5).trim.split(" ")(5).toInt
      Monkey(id, startingItems, operation, (x:BigInt) => if (x % divisor == 0) {nextMonkeyTrue} else {nextMonkeyFalse})
    }).toSeq
  }

  def updateItem(item: BigInt): BigInt = Math.floor(item.toFloat / 3.0).toInt


  def monkeyTurn(monkey: Monkey, allMonkeys: Seq[Monkey], updateFunc: BigInt => BigInt = updateItem): Seq[Monkey] = {

    //inspect
    val updatedOriginalDestination = monkey.items.map(item => (updateFunc(monkey.op.apply(item)) -> item))
      .map(updatedOriginal => (updatedOriginal._1, updatedOriginal._2, monkey.test.apply(updatedOriginal._1)))
    // need to remove old item value from monkey
    updatedOriginalDestination.foldLeft(allMonkeys)((monkeys, update) => {
      val toMonkey = monkeys(update._3).copy(items = monkeys(update._3).items :+ update._1)
      val originalFromMonkey = monkeys(monkey.id)
      val fromMonkey = originalFromMonkey.copy(items = originalFromMonkey.items.filterNot(_ == update._2), inspected = originalFromMonkey.inspected + 1)
      monkeys.updated(update._3, toMonkey).updated(monkey.id, fromMonkey)
    })
  }

  def monkeyRound(monkeys: Seq[Monkey], updateFunc: BigInt => BigInt = updateItem): Seq[Monkey] = {
    monkeys.indices.foldLeft(monkeys)((monkeys, index) => monkeyTurn(monkeys(index), monkeys, updateFunc))
  }

  def monkeyRounds(monkeys: Seq[Monkey], rounds: Int, updateFunc: BigInt => BigInt = updateItem): Seq[Monkey] = {
    (0 until rounds).foldLeft(monkeys)((m, _) => monkeyRound(m, updateFunc))
  }

  def main(args: Array[String]): Unit = {

    val lines = getLines("day11.txt")
    val monkeys = parse(lines)

    val updated = monkeyRounds(monkeys, 20)
    println(s"Part 1: ${updated.map(_.inspected).sorted.takeRight(2).product}")

    val updated2 = monkeyRounds(monkeys, 10000, a => a % (getDivisors(lines).product))
    println(s"Part 2: ${updated2.map(_.inspected).sorted.takeRight(2).product}")
  }
}
