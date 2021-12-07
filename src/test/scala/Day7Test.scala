package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day7Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """16,1,2,0,4,2,7,1,2,14""".stripMargin

    val testData = testInput.split(",").map(_.toInt).toList
  }


  "Day 7 Part 1" should "calculate sum" in {
    Day7.getFuelSum(TestData.testData, 2) should be(37)
  }

  "Day 7 Part 1" should "calculate most efficient" in {
    Day7.mostEfficientPos(TestData.testData, 10000)._1 should be(2)
    Day7.mostEfficientPos(TestData.testData, 10000)._2 should be(37)
  }

  "Day 7 Part 2" should "calculate sum" in {
    Day7.getFuelSum2(TestData.testData, 2) should be(206)
  }

}
