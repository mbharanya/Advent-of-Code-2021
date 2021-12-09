package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

class Day9Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678""".stripMargin

    val testData = testInput.split("\n")

    val map = for {
      x <- (0 until TestData.testData(0).size)
      y <- (0 until TestData.testData.size)
    } yield (x,y) -> TestData.testData(y).split("")(x).toInt
  }


  "Day 9 Part 1" should "get lowest points" in {
    val maze = new Day9.Maze(TestData.map.toMap)
    val lowestPoints = maze.getLowestPoints()
    // lowestPoints.map(_._2) should contain allOf(5,0,5,1)
    lowestPoints.map(_._2) should have size 4
  }
  "Day 9 Part 1" should "get risk level sum" in {
    Day9.getRiskLevel(new Day9.Maze(TestData.map.toMap)) should be(15)
  }

  "Day 9 Part 2" should "get basins" in {
    val maze = new Day9.Maze(TestData.map.toMap)
    val lowest = maze.getLowestPoints()

    val test = maze.getBasin2(Queue.from(List((0,1))), List((0,1)))
    test should have length 3

    val basins = lowest.map(_._1).map((x,y) => maze.getBasin2(Queue.from(List((x,y))), List((x,y))))

    val basinSizes = basins.map(_.size).toList.sorted.reverse.slice(0,3)
//    basinSizes
////     lowest.map(_._1).map(maze.getBasin2(Queue.from(_), Nil)).sorted.reverse.slice(0,3)
    basinSizes.product should be(1134)
  }


}
