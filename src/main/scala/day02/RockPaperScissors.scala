package com.owengilfellon
package day02

import helpers.FileInput


object RockPaperScissors extends FileInput {

  def getMove(otherMove: RpsState, rpsOutcome: RpsOutcome): RpsState = {
   (otherMove, rpsOutcome) match {
      case (_, Draw) => otherMove
      case (Rock, Win) | (Scissors, Lose)  => Paper
      case (Paper, Win) | (Rock, Lose)     => Scissors
      case (Scissors, Win) | (Paper, Lose) => Rock
    }
  }

  def parseStates(char: Char): RpsState = {
    char match {
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
      case _ => throw new IllegalArgumentException(s"$char is not a valid state")
    }
  }

  def parseOutcome(char: Char): RpsOutcome = {
    char match {
      case 'X' => Lose
      case 'Y' => Draw
      case 'Z' => Win
      case _ => throw new IllegalArgumentException(s"$char is not a valid state")
    }
  }

  def cleanStates(input: String): String = input
    .replaceAll(" ", "")
    .replaceAll("\n", "")

  def parseStrategyOne(input: String): (RpsState, RpsState) = {
    val states =  cleanStates(input).map(parseStates)
    assert(states.size == 2)
    (states(0), states(1))
  }

  def parseStrategyTwo(input: String): (RpsState, RpsState) = {
    val states = cleanStates(input)
    assert(states.length == 2)
    val playerOneMove = parseStates(states.head)
    val playerTwoDesiredOutcome = parseOutcome(states(1))
    val playerTwoMove = getMove(playerOneMove, playerTwoDesiredOutcome)
    (playerOneMove,  playerTwoMove)
  }

  def scoreRound(input: (RpsState, RpsState)): (Int, Int) = {

    def scoreWin: (Int, Int) = {
      input match {
        case (Rock, Paper) | (Scissors, Rock) | (Paper, Scissors) => (0, 6)
        case (Paper, Rock) | (Rock, Scissors) | (Scissors, Paper) => (6, 0)
        case (x, y) if x == y => (3, 3)
      }
    }

    val win = scoreWin
    (input._1.score + win._1, input._2.score + win._2)
  }

  def scoreGame(roundScores: Seq[(Int, Int)]): (Int, Int) = {

    def joinTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
      (a._1 + b._1, a._2 + b._2)
    }

    roundScores.fold((0, 0))(joinTuples)
  }

  def getResult(strategy: String => (RpsState, RpsState)): (Int, Int) = {

    val moves = getLines("day02.txt").map(strategy)

    val scores = moves.map(scoreRound)

    scoreGame(scores)

  }

  def main(args: Array[String]): Unit = {

    println(getResult(parseStrategyOne))

    println(getResult(parseStrategyTwo))

  }

}
