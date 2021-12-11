package ch.bharanya

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


  "Day 11 Part 1" should "get step 1 correct" in {
    val octopusMap = TestData.map.map({
      case (x,y) -> value => new Day11.Octopus(x = x, y = y, maybeEnergy = Some(value))
    })
    val tickedOctopi = new Day11.OctopusMap(TestData.map).tickOctopi(Queue.from(octopusMap), Nil)
    tickedOctopi
  }

}
