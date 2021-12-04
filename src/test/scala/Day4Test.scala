package ch.bharanya

import ch.bharanya.Day4.Board
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day4Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        |
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7""".stripMargin

    val testData = testInput.split("\n").toList
  }


  "Day 4 Part 1" should "calculate getMostCommonBits" in {
    val winningNumbers = TestData.testData.head.split(",")
    val boards = Day4.createBoards(TestData.testData.tail.tail)
    boards should have length 3

    val winners = Day4.checkWinningNumbers(winningNumbers.map(_.toInt).toList, boards)
    val result = winners.head match {
      case (i, board, winNumbers) => board.head.getWinScore(winNumbers)
    }
    result should be(4512)
  }


  "Day 4 Part 2" should "calculate getMostCommonBits" in {
    Day4.part2(TestData.testData) should be(1924)
  }
}
