package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day3Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin

    val testData = testInput.split("\n").toList
  }


  "Day 3 Part 1" should "calculate test data" in {
    Day2.getMultipliedPositionDepth(TestData.testData) should be(150)
  }

  "Day 3 Part 2" should "calculate test data" in {
    Day2.getWithAim(TestData.testData) should be(900)
  }

}
