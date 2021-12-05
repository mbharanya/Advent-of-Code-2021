package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack


class Day5Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """0,9 -> 5,9
        |8,0 -> 0,8
        |9,4 -> 3,4
        |2,2 -> 2,1
        |7,0 -> 7,4
        |6,4 -> 2,0
        |0,9 -> 2,9
        |3,4 -> 1,4
        |0,0 -> 8,8
        |5,5 -> 8,2""".stripMargin

    val testData = testInput.split("\n").toList
  }


  "Day 5 Part 1" should "calculate getCoveredCoords" in {
    val coords = new Day5.Line(Day5.Position(1,1), Day5.Position(1,3)).getCoveredCoords()
    coords.toList(0) should be(Day5.Position(1,1))
    coords.toList(1) should be(Day5.Position(1,2))
    coords.toList(2) should be(Day5.Position(1,3))

    val coords2 = new Day5.Line(Day5.Position(9,7), Day5.Position(7,7)).getCoveredCoords()
    coords2 should contain allOf(Day5.Position(9,7), Day5.Position(8,7), Day5.Position(7,7))

    new Day5.Line(Day5.Position(9,7), Day5.Position(7,9)).getCoveredCoords() should be(Seq(Day5.Position(9,7), Day5.Position(8,8), Day5.Position(7,9)))
    new Day5.Line(Day5.Position(1,1), Day5.Position(3,3)).getCoveredCoords() should be(Seq(Day5.Position(1,1), Day5.Position(2,2), Day5.Position(3,3)))
  }

  "Day 5 Part 1" should "calculate overlaps" in {
    val matrix = Day5.part1(TestData.testData)

    println(matrix)
    val filter: Day5.Line => Boolean = l => l.from.x == l.to.x || l.from.y == l.to.y
    val intersections = matrix.getIntersections(2, filter)
    intersections.keys.toList should have length(5)
  }


  "Day 5 Part 2" should "calculate overlaps" in {
    val matrix = Day5.part1(TestData.testData)

    println(matrix)
    val filter: Day5.Line => Boolean = _ => true
    val intersections = matrix.getIntersections(2, filter)
    println(matrix.plot(_ => true))
    intersections.keys.toList should have length(12)
  }

}
