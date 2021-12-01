package ch.bharanya

import scala.io.Source

object Day1 extends App {



  def getAmountIncreased(measurements: List[Int]) = measurements
    .zipWithIndex
    .map((m, i) => m > measurements.lift(i - 1).getOrElse(Integer.MAX_VALUE))
    .filter(_ == true)
    .length


  def getAmountIncreasedSliding(measurements: List[Int]) = measurements
    .zipWithIndex
    .map((m, i) => {
      val previousSlidingWindow = for {
        first <- measurements.lift(i - 1)
        second <- measurements.lift(i)
        third <- measurements.lift(i + 1)
      } yield first + second + third

      val nextSlidingWindow = for {
        first <- measurements.lift(i)
        second <- measurements.lift(i + 1)
        third <- measurements.lift(i + 2)
      } yield first + second + third

      val isLarger = for {
        next <- nextSlidingWindow
        previous <- previousSlidingWindow
      } yield next > previous

      isLarger.getOrElse(false)
    })
    .filter(_ == true)
    .length



  val filename = "res/day1_1.txt"
  val measurementFromFile = Source.fromFile(filename).getLines.map(_.toInt).toList

  println(getAmountIncreased(measurementFromFile))

  println(getAmountIncreasedSliding(measurementFromFile))

  println("Golfed:")
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
