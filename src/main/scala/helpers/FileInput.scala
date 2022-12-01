package com.owengilfellon
package helpers

import scala.io.Source

trait FileInput {

  def getLines(fileName: String): Seq[String] = {
    val reader = Source.fromFile(s"src/main/resources/$fileName")
    val fileLines: Seq[String] = reader.getLines.toSeq
    reader.close()
    fileLines
  }

}
