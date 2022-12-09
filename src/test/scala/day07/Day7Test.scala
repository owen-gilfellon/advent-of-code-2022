package com.owengilfellon
package day07

import com.owengilfellon.day07.Day7.{Cursor, DiskItem, getSizedDirs, processOutput}
import com.owengilfellon.helpers.BaseSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class Day7Test extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """$ cd /
                                     |$ ls
                                     |dir a
                                     |14848514 b.txt
                                     |8504156 c.dat
                                     |dir d
                                     |$ cd a
                                     |$ ls
                                     |dir e
                                     |29116 f
                                     |2557 g
                                     |62596 h.lst
                                     |$ cd e
                                     |$ ls
                                     |584 i
                                     |$ cd ..
                                     |$ cd ..
                                     |$ cd d
                                     |$ ls
                                     |4060174 j
                                     |8033020 d.log
                                     |5626152 d.ext
                                     |7214296 k"""

  "Day7" - {

    "parseOutput should return list of files with correct paths" in {

      val files = inputSeq.foldLeft(Cursor())(processOutput).files
      files.head mustBe DiskItem("/b.txt", 14848514L)
      files(1) mustBe DiskItem("/c.dat", 8504156)
      files(2) mustBe DiskItem("/a/f", 29116L)
      files(3) mustBe DiskItem("/a/g", 2557L)
      files(4) mustBe DiskItem("/a/h.lst", 62596L)
      files(5) mustBe DiskItem("/a/e/i", 584L)
      files(6) mustBe DiskItem("/d/j", 4060174L)
      files(7) mustBe DiskItem("/d/d.log", 8033020L)
      files(8) mustBe DiskItem("/d/d.ext", 5626152L)
      files(9) mustBe DiskItem("/d/k", 7214296L)


    }

    "getSizedDirs should return correct directory sizes" in {

      val files = inputSeq.foldLeft(Cursor())(processOutput).files
      val dirs = getSizedDirs(files).filter(_.size <= 100000).sortBy(_.size)
      dirs.head mustBe DiskItem("/a/e",  584L)
      dirs(1) mustBe DiskItem("/a", 94853L)

    }
  }
}
