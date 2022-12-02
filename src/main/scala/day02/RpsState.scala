package com.owengilfellon
package day02

sealed trait RpsState {
  val score: Int
}

case object Rock extends RpsState {
  val score = 1
}
case object Paper extends RpsState{
  val score = 2
}
case object Scissors extends RpsState{
  val score = 3
}
