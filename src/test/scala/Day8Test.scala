package ch.bharanya

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class Day8Test extends AnyFlatSpec with should.Matchers {
  object TestData{
    val testInput: String =
      """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
        |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
        |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
        |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
        |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
        |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
        |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
        |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
        |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
        |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin

    val testData = testInput.split("\n")
  }


  "Day 8 Part 1" should "get four digit output" in {
    val sum = TestData.testData.map(line => {
      val input = line.split(" \\| ")(0)
      val output = line.split(" \\| ")(1)
      Day8.getAmountUniqueDigits(input.split(" ").toList, output.split(" ").toList, List(1,4,7,8))
    }).sum
    sum should be(26)
  }


  "Day 8 Part 2" should "get digit" in {
    TestData.testData.map(line => {
      val input = line.split(" \\| ")(0)
      val output = line.split(" \\| ")(1)
      val res = Day8.getDigit(input.split(" ").toList, output.split(" ").toList)
      res
    }).sum should be(61229)
  }

}
