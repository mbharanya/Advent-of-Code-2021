package ch.bharanya

object Day8 extends App{
  val numToSegmentElementAmount = Map(
    0 -> 6,
    1 -> 2,
    2 -> 5,
    3 -> 5,
    4 -> 4,
    5 -> 5,
    6 -> 6,
    7 -> 3,
    8 -> 7,
    9 -> 6,
  )

  val segmentValueAmountToNum = numToSegmentElementAmount.groupMapReduce(identity)(_ => 1)(_ + _)

  def getAmountUniqueDigits(input: List[String], output: List[String], digits: List[Int]): Int = {
      val onlySelectedDigits = output.map(seg => seg.length).filter(l =>
        digits.exists(d => l == numToSegmentElementAmount(d))
      )
    onlySelectedDigits.size
  }

  def getDigit(input: List[String], output: List[String]): Int = {
    val onlySelectedDigits = output.map(seg => seg.length).filter(l =>
      digits.exists(d => l == numToSegmentElementAmount(d))
    )
    onlySelectedDigits.size
  }


  def part1() = {
    val lines = Util.getSplitLine(8, "\n")
    val sum = lines.map(line => {
      val input = line.split(" \\| ")(0)
      val output = line.split(" \\| ")(1)
      Day8.getAmountUniqueDigits(input.split(" ").toList, output.split(" ").toList, List(1,4,7,8))
    }).sum
    println(sum)
  }

  part1()
}
