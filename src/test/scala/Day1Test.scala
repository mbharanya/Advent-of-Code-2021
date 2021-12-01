package ch.bharanya

import org.scalactic.Prettifier.default

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*

class Day1Test extends AnyFlatSpec with should.Matchers {

  object TestData{
    val testInput: String =
      """199
        |200
        |208
        |210
        |200
        |207
        |240
        |269
        |260
        |263""".stripMargin

    val testMeasurements = testInput.split("\n").map(_.toInt).toList
  }


  "Day 1 Part 1" should "calculate test data" in {
    Day1.getAmountIncreased(TestData.testMeasurements) should be(7)
  }

  "Day 1 Part 2" should "should calculate with test data" in {
    Day1.getAmountIncreasedSliding(TestData.testMeasurements) should be(5)
  }
}
