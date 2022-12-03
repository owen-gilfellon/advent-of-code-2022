package com.owengilfellon
package helpers

trait BaseSpec {


  val testInput: String

  def inputSeq: Seq[String] = testInput.stripMargin.split("\n").toSeq

}
