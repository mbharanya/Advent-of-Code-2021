package ch.bharanya

import ch.bharanya.Day11.Coordinate
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

class Day11Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526""".stripMargin

    val testData = testInput.split("\n")
    val map = (for {
      x <- (0 until TestData.testData(0).size)
      y <- (0 until TestData.testData.size)
    } yield (x,y) -> TestData.testData(y).split("")(x).toInt).toMap
  }


  "Day 11 Part 1" should "get steps correct" in {
    val results = Day11.part1(TestData.testData.toList)
    results(0).get(Coordinate(0,0)) should be(Some(5))
    results(1).get(Coordinate(0,0)) should be(Some(6))
    results(2).get(Coordinate(0,0)) should be(Some(8))
    results(10).get(Coordinate(0,0)) should be(Some(0))


    results.take(3).map(_.values.count(_ == 0)).sum should be(35)


    results.take(11).map(_.values.count(_ == 0)).sum should be(204)
  }

  "Day 11 Part 2" should "get step when all is 0" in {
    val results = Day11.part2(TestData.testData.toList) should be(195)
  }


}
