package com.owengilfellon
package day02

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class RockPaperScissorsTest  extends AnyFreeSpec {

  val testInput = """A Y
                    |B X
                    |C Z""".stripMargin.split("\n").toSeq

  "parseStrategyOne" - {

    val states = testInput.map(RockPaperScissors.parseStrategyOne)

    "must return correct states" in {
      states.size mustBe 3
      states.head mustBe (Rock, Paper)
      states(1) mustBe (Paper, Rock)
      states(2) mustBe (Scissors, Scissors)
    }

    val scores = states.map(RockPaperScissors.scoreRound)

    "states must be scored correctly" in {
      scores.size mustBe 3
      scores.head mustBe (1, 8)
      scores(1) mustBe (8, 1)
      scores(2) mustBe (6, 6)
    }

    "game must be scored correctly" in {
      RockPaperScissors.scoreGame(scores)._2 mustBe 15
    }
  }

  "parseStrategyTwo" - {

    val states = testInput.map(RockPaperScissors.parseStrategyTwo)

    "must return correct states" in {
      states.size mustBe 3
      states.head mustBe (Rock, Rock)
      states(1) mustBe (Paper, Rock)
      states(2) mustBe (Scissors, Rock)
    }

    val scores = states.map(RockPaperScissors.scoreRound)


    "states must be scored correctly" in {
      scores.size mustBe 3
      scores.head._2 mustBe 4
      scores(1)._2 mustBe 1
      scores(2)._2 mustBe 7
    }

    "game must be scored correctly" in {
      RockPaperScissors.scoreGame(scores)._2 mustBe 12
    }
  }
}
