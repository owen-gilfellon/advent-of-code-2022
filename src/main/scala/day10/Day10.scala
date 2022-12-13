package com.owengilfellon
package day10

import helpers.FileInput

import scala.annotation.tailrec

object Day10 extends FileInput {

  sealed trait Command {
    val cycles: Int
  }

  case class Add(x: Int) extends Command {
    val cycles = 2
  }
  case object NoOp extends Command {
    val cycles = 1
  }

  case class Register(cycle: Int, x: Int)

  def parseCommand(line: String): Command = {
   if( line.startsWith("addx")) {
      Add(line.split(" ")(1).toInt)
    } else
     NoOp
  }

  def executeUntil(register: Register, executeUntil: Int, commands: Seq[Command]): Register = {
    @tailrec
    def getCommands(cycle: Int, executed: Seq[Command], commands: Seq[Command]): Seq[Command] = {
      if(cycle + commands.head.cycles > executeUntil)
        executed
      else
        getCommands(cycle + commands.head.cycles, executed :+ commands.head, commands.tail)
    }

    applyCommands(register, getCommands(1, Seq(), commands))
  }

  def applyCommands(register: Register, commands: Seq[Command]): Register = {
    commands.foldLeft(register)((register, command) => command match {
      case Add(value) => register.copy(x = register.x + value, cycle = register.cycle + command.cycles)
      case x => register.copy(cycle = register.cycle + x.cycles)
    })
  }

  def main(args: Array[String]): Unit = {

    def strength(x: Int):Int = executeUntil(Register(1, 1), x, getLines("day10.txt").map(parseCommand)).x * x

    println(s"Part 1: ${Seq(20, 60, 100, 140, 180, 220).map(strength).sum}")

    val toPrint = (1 to 240).map(i => {
        val pixel = i % 40
        val value =  executeUntil(Register(1, 2), i, getLines("day10.txt").map(parseCommand)).x
        val s = if(value >= pixel - 1 && value <= pixel + 1) { "#" } else { "."}
        if(pixel == 0) s"$s\n" else s
    }).mkString

    println(s"Part 2:\n ${toPrint}")
    // EJCFPGLH
  }

}
