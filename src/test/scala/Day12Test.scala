package ch.bharanya

import Day11.Coordinate

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

class Day12Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """start-A
        |start-b
        |A-c
        |A-b
        |b-d
        |A-end
        |b-end""".stripMargin

    val testData = testInput.split("\n").toList

  }


  "Day 12 Part 1" should "get ConnectionMap" in {
    val conMap = Day12.getConnections(TestData.testData)
    conMap.size should be(TestData.testData.size * 2)
  }

  it should "step" in {
    val conMap = Day12.getConnections(TestData.testData)
//    Day12.step()
  }

  "Day 12 Part 2" should "get step when all is 0" in {
  }


}
