package ch.bharanya

import scala.io.Source

object Util {
    def getFileLines(dayNumber: Int): List[String] = {
      val filename = s"res/day${dayNumber}.txt"
      Source.fromFile(filename).getLines.toList
    }

  def getSplitLine(dayNumber: Int, separator: String): List[String] = {
    val filename = s"res/day${dayNumber}.txt"
    Source.fromFile(filename).getLines.flatMap(_.split(separator)).toList
  }

}
