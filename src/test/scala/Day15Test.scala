package ch.bharanya

import Day15.Coordinate

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

class Day15Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """1163751742
        |1381373672
        |2136511328
        |3694931569
        |7463417111
        |1319128137
        |1359912421
        |3125421639
        |1293138521
        |2311944581""".stripMargin

    val testData = testInput.split("\n").toList

  }


  "Day 15 Part 1" should "get map" in {
    val map = Day15.getMap(TestData.testData)
    map(Coordinate(0,0)) should be(1)
    map(Coordinate(1,1)) should be(3)
    map(Coordinate(9,9)) should be(1)
    map.lift(Coordinate(10,10)) should be(None)
  }

  it should "get neighbors" in {
    val result = Day15.part1(TestData.testData, new Coordinate(9,9))
    result
  }

}
