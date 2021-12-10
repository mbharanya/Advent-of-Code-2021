package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

class Day10Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """[({(<(())[]>[[{[]{<()<>>
        |[(()[<>])]({[<{<<[]>>(
        |{([(<{}[<>[]}>{[]{[(<()>
        |(((({<>}<{<{<>}{[]{[]{}
        |[[<[([]))<([[{}[[()]]]
        |[{[{({}]{}}([{[{{{}}([]
        |{<[[]]>}<{[{[{[]{()[[[]
        |[<(<(<(<{}))><([]([]()
        |<{([([[(<>()){}]>(<<{{
        |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

    val testData = testInput.split("\n")
  }


  "Day 10 Part 1" should "get incomplete and invalid" in {
    val results = Day10.iterate("[({(<(())[]>[[{[]{<()<>>".toCharArray.toList, Nil)
    results match {
      case Day10.Incomplete(missing) => missing should not be empty
    }
    Day10.iterate("{([(<{}[<>[]}>{[]{[(<()>".toCharArray.toList, Nil). match {
      case Day10.Invalid(expected, actual) => {
        actual should be('}')
        expected should be(']')
      }
    }
  }

  "Day 10 Part 1" should "score" in {
    Day10.part1(TestData.testData.toList) should be (26397)
  }


  "Day 10 Part 2" should "get completions" in {
    val completions = Day10.getCompletions("[({(<(())[]>[[{[]{<()<>>".toCharArray.toList)

    completions.mkString should be("}}]])})]")

    Day10.getValueOfCompletionString(completions) should be(288957)
  }

  "Day 10 Part 2" should "get score" in {
    Day10.part2(TestData.testData.toList) should be(288957)
  }


}
