package ch.bharanya

object Day8 extends App {
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

  val possibleOthers = Map(
    0 -> List(0, 1, 7),
    1 -> List(),
    6 -> List(5, 6),
    9 -> List(1, 3, 4, 5, 7, 9)
  )

  /*
    0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

  */

  val segmentValueAmountToNum = numToSegmentElementAmount.groupMapReduce(identity)(_ => 1)(_ + _)

  def getAmountUniqueDigits(input: List[String], output: List[String], digits: List[Int]): Int = {
    val onlySelectedDigits = output.map(seg => seg.length).filter(l =>
      digits.exists(d => l == numToSegmentElementAmount(d))
    )
    onlySelectedDigits.size
  }


  def getDigit(input: List[String], output: List[String]): Int = {
    var one: Set[Char] = Set()
    var four: Set[Char] = Set()
    var seven: Set[Char] = Set()

    input.foreach(segment => {
      val segmentLength = segment.length
      val chars = segment.toCharArray.toSet

      if (segmentLength == 2) one = chars
      if (segmentLength == 4) four = chars
      if (segmentLength == 7) seven = chars
    })

    val segmentToNumbers = output.map(segment => {
      val chars = segment.toCharArray.toSet

      var number = -1
      val intersectionsWithOne = chars.intersect(one)
      val intersectionsWithFour = chars.intersect(four)
      val intersectionsWithSeven = chars.intersect(seven)

      if (segment.length == 6) {
        if (intersectionsWithOne.size == 1) {
          number = 6
        } else if (intersectionsWithFour.size == 4) {
          number = 9
        } else {
          number = 0
        }
      }

      if (segment.length == 5) {
        if (intersectionsWithFour.size == 2) {
          number = 2
        } else if (intersectionsWithOne.size == 1) {
          number = 5
        } else {
          number = 3
        }
      }

      if (segment.length == 2) number = 1
      if (segment.length == 3) number = 7
      if (segment.length == 4) number = 4
      if (segment.length == 7) number = 8

      segment -> number
    })

    println(segmentToNumbers.map(_._2).mkString)

    segmentToNumbers.map(_._2).mkString.toInt
  }


  def part1() = {
    val lines = Util.getSplitLine(8, "\n")
    val sum = lines.map(line => {
      val input = line.split(" \\| ")(0)
      val output = line.split(" \\| ")(1)
      Day8.getAmountUniqueDigits(input.split(" ").toList, output.split(" ").toList, List(1, 4, 7, 8))
    }).sum
    println(sum)
  }

  def part2() = {
    val lines = Util.getSplitLine(8, "\n")
    val sum = lines.map(line => {
      val input = line.split(" \\| ")(0)
      val output = line.split(" \\| ")(1)
      Day8.getDigit(input.split(" ").toList, output.split(" ").toList)
    }).sum
    println(sum)

  }

  part1()
  part2()
}
