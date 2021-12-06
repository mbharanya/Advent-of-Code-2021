package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day6Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """3,4,3,1,2""".stripMargin

    val testData = testInput.split(",").map(_.toLong).toList
  }


  "Day 6 Part 1" should "calculate State" in {
    Day6.tick(TestData.testData,1, 18) should have length 26
    Day6.tick(TestData.testData,1, 80) should have length 5934
  }

  "Day 6 Part 2" should "calculate State" in {
    Day6.tick(TestData.testData,1, 256) should have length 26984457539
  }

}
