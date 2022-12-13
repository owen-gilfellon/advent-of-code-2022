package com.owengilfellon
package day11

import helpers.BaseSpec

import com.owengilfellon.day11.Day11.getDivisors
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class Day11Test extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """Monkey 0:
                                     |  Starting items: 79, 98
                                     |  Operation: new = old * 19
                                     |  Test: divisible by 23
                                     |    If true: throw to monkey 2
                                     |    If false: throw to monkey 3
                                     |
                                     |Monkey 1:
                                     |  Starting items: 54, 65, 75, 74
                                     |  Operation: new = old + 6
                                     |  Test: divisible by 19
                                     |    If true: throw to monkey 2
                                     |    If false: throw to monkey 0
                                     |
                                     |Monkey 2:
                                     |  Starting items: 79, 60, 97
                                     |  Operation: new = old * old
                                     |  Test: divisible by 13
                                     |    If true: throw to monkey 1
                                     |    If false: throw to monkey 3
                                     |
                                     |Monkey 3:
                                     |  Starting items: 74
                                     |  Operation: new = old + 3
                                     |  Test: divisible by 17
                                     |    If true: throw to monkey 0
                                     |    If false: throw to monkey 1"""

  "Day11" - {

    val monkeys = Day11.parse(inputSeq)

    "parse should parse monkeys" in {
      monkeys.head.items mustBe Seq(79, 98)
      monkeys(3).items mustBe Seq(74)
    }

    "monkeyTurn should perform one turn" in {
      val updatedMonkeys = Day11.monkeyTurn(monkeys.head, monkeys)
      updatedMonkeys.head.items mustBe Seq()
      updatedMonkeys(3).items mustBe Seq(74, 500, 620)
    }

    "monkeyRound" - {

      "in part one" - {
        "should perform one round" in  {
          val updateMonkeys = Day11.monkeyRound(monkeys)
          updateMonkeys.head.items mustBe Seq(20, 23, 27, 26)
          updateMonkeys(1).items mustBe Seq(2080, 25, 167, 207, 401, 1046)
          updateMonkeys(2).items mustBe Seq()
          updateMonkeys(3).items mustBe Seq()
        }

        "should perform 20 rounds" in {
          val updateMonkeys = (1 to 20).foldLeft(monkeys)((a, _) => Day11.monkeyRound(a))
          updateMonkeys.head.items mustBe Seq(10, 12, 14, 26, 34)
          updateMonkeys(1).items mustBe Seq(245, 93, 53, 199, 115)
          updateMonkeys(2).items mustBe Seq()
          updateMonkeys(3).items mustBe Seq()
        }

        "should return correct number of inspections" in {
          Day11.monkeyRounds(monkeys, 20).map(_.inspected) mustBe Seq(101, 95, 7, 105)
        }
      }

      "in part two should return correct " - {

        "inspections after" - {
          "round 1" in {
            Day11.monkeyRounds(monkeys, 1, a => a % (getDivisors(inputSeq).product)).map(_.inspected) mustBe Seq(2, 4, 3, 6)
          }
          "round 20" in {
            Day11.monkeyRounds(monkeys, 20, a => a % (getDivisors(inputSeq).product)).map(_.inspected) mustBe Seq(99, 97, 8, 103)
          }
          "round 1000" in {
            Day11.monkeyRounds(monkeys, 1000, a => a % (getDivisors(inputSeq).product)).map(_.inspected) mustBe Seq(5204, 4792, 199, 5192)
          }
          "round 2000" in {
            Day11.monkeyRounds(monkeys, 2000, a => a % (getDivisors(inputSeq).product)).map(_.inspected) mustBe Seq(10419, 9577, 392, 10391)
          }
          "round 10000" in {
            Day11.monkeyRounds(monkeys, 10000, a => a % (getDivisors(inputSeq).product)).map(_.inspected) mustBe Seq(52166, 47830, 1938, 52013)
          }
        }

        "monkey business after round 10000" in {
          Day11.monkeyRounds(monkeys, 10000, a => a % (getDivisors(inputSeq).product)).map(_.inspected).sorted.takeRight(2).product mustBe BigInt(2713310158L)
        }
      }



    }


  }
}
