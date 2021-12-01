import Day1.testMeasurements

import scala.io.Source

object Day1 extends App {
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

  val testMeasurements = testInput.split("\n").map(_.toInt).toList
  println(getAmountIncreased(testMeasurements))


  val filename = "res/day1_1.txt"
  val measurementFromFile = Source.fromFile(filename).getLines.map(_.toInt).toList

  println(getAmountIncreased(measurementFromFile))

  println(getAmountIncreasedSliding(testMeasurements))
  println(getAmountIncreasedSliding(measurementFromFile))


}
