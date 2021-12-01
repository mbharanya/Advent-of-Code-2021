package ch.bharanya

import scala.io.Source

object Day1_Golfed extends App {
  val testInput: String =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  val testMeasurements = testInput.split("\n").map(_.toInt).toList

  val filename = "res/day1_1.txt"
  val measurementFromFile = Source.fromFile(filename).getLines.map(_.toInt).toList


  println(
    measurementFromFile.sliding(2, 1)
      .count(l => l(0) < l(1))
  )

  println(
    measurementFromFile.sliding(3, 1)
      .map(_.sum)
      .sliding(2,1)
      .count(l => l(0) < l(1))
  )

}
