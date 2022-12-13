package com.owengilfellon
package day07

import helpers.FileInput

import scala.annotation.tailrec

object Day7 extends FileInput {

  case class Cursor(location: String = "", files: Seq[DiskItem] = Seq())

  case class DiskItem(path: String, size: Long)

  object DiskItem {

    private val filePattern = "([0-9]+) ([a-zA-Z0-9\\.]+)".r

    def apply(location: String, line: String): Option[DiskItem] = {
      filePattern.findFirstMatchIn(line) match {
        case Some(m) => (m.group(1), m.group(2)) match {
          case (fileSize, fileName) => Some(DiskItem(s"${location}/$fileName", fileSize.toLong))
        }
        case _ => None
      }
    }
  }

  def processOutput(cursor: Cursor, line: String): Cursor = {
    if(line.startsWith("$ cd")) {
      val arg = line.split(" ").apply(2).trim
      cursor.copy(location = arg match {
        case ".." => s"${cursor.location.take(cursor.location.lastIndexOf('/')  )}"
        case "/" => ""
        case _ if cursor.location.endsWith("/") => s"${cursor.location}$arg"
        case _ => s"${cursor.location}/$arg"
      })
    } else {
      cursor.copy(files = cursor.files :++ Seq(DiskItem.apply(cursor.location, line)).flatten)
    }
  }

  def getSizedDirs(files: Seq[DiskItem]): Seq[DiskItem] = {

    def getDirsFromFilePath(f: DiskItem): Seq[DiskItem] = {

      @tailrec
      def getDirsRec(path: String, fileSize: Long, dirs: Seq[DiskItem]): Seq[DiskItem] = {
        if(path.isEmpty)
          dirs else {
          val dir = path.substring(0, path.lastIndexOf('/'))
          getDirsRec(dir, fileSize, dirs :+ DiskItem(dir, fileSize))
        }
      }

      getDirsRec(f.path, f.size, Seq())
    }

    files.flatMap(getDirsFromFilePath)
      .groupBy(_.path)
      .map(s => s._2.reduce((a, b) => a.copy(size = a.size + b.size)))
      .toSeq
  }

  def main(args: Array[String]): Unit = {
    val lines = getLines("day07.txt")
    val answer1 = getSizedDirs(lines.foldLeft(Cursor())(processOutput).files).filter(_.size <= 100000).map(_.size).sum
    println(s"Part 1: ${answer1}")

    val answer2 = lines.foldLeft(Cursor())(processOutput).files.map(_.size).sum
    val answer3 = getSizedDirs(lines.foldLeft(Cursor())(processOutput).files)

    val totalSpace = 70000000L
    val requiredSpace = 30000000L
    val unusedSpace = totalSpace - answer2

    println(s"Part 2: ${answer3.map(a => (a, a.size + unusedSpace)).filter(a => a._2 >= requiredSpace).minBy(_._1.size)._1.size}")
  }
}
