package ch.bharanya

import scala.io.Source

object Util {
    def getFileLines(dayNumber: Int): List[String] = {
      val filename = s"res/day${dayNumber}.txt"
      Source.fromFile(filename).getLines.toList
    }

}
