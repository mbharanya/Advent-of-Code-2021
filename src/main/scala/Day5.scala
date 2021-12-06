package ch.bharanya

import scala.io.Source

object Day5 extends App {
  case class Position(x: Int, y: Int)

  class Line(val from: Position, val to: Position) {
    def getCoveredCoords(): Seq[Position] = {
      val lineLength = Math.max(Math.abs(from.x - to.x), Math.abs(from.y - to.y))
      val (directionX, directionY) = ((to.x - from.x).sign, (to.y - from.y).sign)

      (0 to lineLength).map(i => {
        Position(
          from.x + (i * directionX),
          from.y + (i * directionY)
        )
      })
    }

    def ==(a: Line, b: Line) = a.from == b.from && a.to == b.to

    override def toString: String = s"${from} -> ${to} (${getCoveredCoords().length} items)"
  }

  class Matrix(lines: Seq[Line]) {
    def getIntersections(numOverlaps: Int, filterCondition: Line => Boolean): Map[Position, Int] = {
      val validLines = lines.filter(filterCondition)
      val coords = validLines.flatMap(_.getCoveredCoords())
      val grouped: Map[Position, Int] = coords.groupBy(identity).view.mapValues(_.size).toMap.filter((_, l) => l >= numOverlaps)
      grouped
    }

    def plot(filterCondition: Line => Boolean): String = (for {
      y <- (0 until 9)
      x <- (0 until 9)
    } yield {
      val intersections = getIntersections(2, filterCondition)
      val currentPos = Position(x, y)
      val separator = if (x == 8) "\n" else ""
      val str = if (intersections.keys.toList.contains(currentPos)) {
        intersections(currentPos).toString
      } else {
        "."
      }
      str + separator
    }).mkString("")
  }

  def getMatrix(input: List[String]): Matrix = {
    val Pattern = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    new Matrix(input.map { line =>
      line match {
        case Pattern(x1, y1, x2, y2) =>
          Line(Position(x1.toInt, y1.toInt), Position(x2.toInt, y2.toInt))
      }
    })
  }

  def part1(input: List[String]) ={
    val filter: Day5.Line => Boolean = l => l.from.x == l.to.x || l.from.y == l.to.y

    val matrix = getMatrix(Util.getFileLines(5))
    val intersections = matrix.getIntersections(2, filter)
    println(intersections.keys.size)
  }

  def part2(input: List[String]) ={
    val matrix2 = getMatrix(Util.getFileLines(5))
    val intersections2 = matrix2.getIntersections(2, _ => true)
    println(intersections2.keys.size)
  }




//  val matrixBig = getMatrix(Source.fromFile("res/day5_big.txt").getLines.toList)
//  val intersections = matrixBig.getIntersections(2, _ => true)
//  println(intersections.keys.size)
//

  val filter: Day5.Line => Boolean = l => l.from.x == l.to.x || l.from.y == l.to.y

  val t1 = System.nanoTime



  val matrixBig = getMatrix(Source.fromFile("res/5-50000-10000.in").getLines.toList)
  println(matrixBig.getIntersections(2, filter).keys.size)
  println(matrixBig.getIntersections(2, _ => true).keys.size)

  val duration = (System.nanoTime - t1) / 1e9d

  println(duration)
}
