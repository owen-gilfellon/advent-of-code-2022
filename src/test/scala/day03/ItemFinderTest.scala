package com.owengilfellon
package day03

import com.owengilfellon.helpers.BaseSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class ItemFinderTest extends AnyFreeSpec with BaseSpec {

  override val testInput: String = """vJrwpWtwJgWrhcsFMMfFFhFp
                                     |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                                     |PmmdzqPrVvPwwTWBwg
                                     |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                                     |ttgJtRGJQctTZtZT
                                     |CrZsJsPPZsGzwwsLwLmpwMDw"""

  "ItemFinder" - {


    "find" - {

      "must identify correct item in valid input" in {

        val items = inputSeq.map(ItemFinder.find)

        items.size mustBe 6
        items mustBe Seq('p', 'L', 'P', 'v', 't', 's')
      }

      "must throw IllegalArgumentException if there is no common item" in {
        assertThrows[IllegalArgumentException](ItemFinder.find("xY"))
      }

      "must throw AssertionError if compartments are not the same size" in {
        assertThrows[AssertionError](ItemFinder.find("x"))
      }

    }

    "findBadge" - {

      "must identify correct badge in first group" in {
        ItemFinder.findBadge(inputSeq.take(3)) mustBe 'r'
      }

      "must identify correct badge in second group" in {
        ItemFinder.findBadge(inputSeq.drop(3)) mustBe 'Z'
      }

      "must throw IllegalArgumentException if there is no badge" in {
        assertThrows[IllegalArgumentException](ItemFinder.findBadge(Seq("a", "a", "b")))
      }

    }

    "priority" - {

      "must map a to 1" in {
        ItemFinder.priority('a') mustBe 1
      }
      "must map z to 26" in {
        ItemFinder.priority('z') mustBe 26
      }
      "must map A to 27" in {
        ItemFinder.priority('A') mustBe 27
      }
      "must map Z to 52" in {
        ItemFinder.priority('Z') mustBe 52
      }
    }

    "sumPriorities must be 157 for test input" in {
      ItemFinder.sumPriorities(inputSeq) mustBe 157
    }
  }
}
