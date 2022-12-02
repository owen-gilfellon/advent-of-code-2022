package com.owengilfellon
package day02

sealed trait RpsOutcome {

}

case object Lose extends RpsOutcome {
  val score = 1
}
case object Draw extends RpsOutcome{
  val score = 2
}
case object Win extends RpsOutcome{
  val score = 3
}
