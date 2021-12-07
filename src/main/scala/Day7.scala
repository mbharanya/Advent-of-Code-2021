package ch.bharanya

object Day7 extends App{

  def getFuelSum(crabs: List[Int], goalPos: Int) = {
    crabs.map(c => Math.abs(c - goalPos)).sum
  }

  def getFuelSum2(crabs: List[Int], goalPos: Int) = {
    crabs.map(c => {
      val steps = Math.abs(c - goalPos)
      factorialPlus(steps)
    }).sum
  }

  def factorialPlus(n: Int) = (n* (n + 1)) / 2

  def mostEfficientPos(crabs: List[Int], maxCount: Int) = {
    val map = (0 until maxCount).map(i => i -> getFuelSum(crabs, i)).toMap
    map.minBy(_._2)
  }

  def mostEfficientPos2(crabs: List[Int], maxCount: Int) = {
    val map = (0 until maxCount).map(i => i -> getFuelSum2(crabs, i)).toMap
    map.minBy(_._2)
  }

  println(mostEfficientPos(Util.getSplitLine(7, ",").map(_.toInt), 1000))
  println(mostEfficientPos2(Util.getSplitLine(7, ",").map(_.toInt), 1000))

}
