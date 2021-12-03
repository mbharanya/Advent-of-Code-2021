package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day3Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """00100
        |11110
        |10110
        |10111
        |10101
        |01111
        |00111
        |11100
        |10000
        |11001
        |00010
        |01010""".stripMargin

    val testData = testInput.split("\n").toList
  }


  "Day 3 Part 1" should "calculate getMostCommonBits" in {
    Day3.getMostCommonBits(TestData.testData) should be(22)
  }
  "Day 3 Part 1" should "calculate getLeastCommonBits" in {
    Day3.getLeastCommonBits(TestData.testData) should be(9)
  }

  "Day 3 Part 1" should "calculate result" in {
    Day3.getPart1(TestData.testData)  should be(198)
  }

}
