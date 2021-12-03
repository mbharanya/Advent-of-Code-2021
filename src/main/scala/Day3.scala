package ch.bharanya

object Day3 extends App {
  def getMostCommonBits(lines: List[String]): Int = {
    val columns = (0 until lines(0).length).map { charIndex =>
      val charsForLine = (0 until lines.length).map { i =>
        lines(i).charAt(charIndex)
      }
      charsForLine
    }

    val groupedBySize = columns.map {
      column => {
        val mapped = column.groupBy(identity).view.mapValues(_.size).toMap
        if (mapped.values.toList(0) > mapped.values.toList(1))
          mapped.keys.toList(0)
        else
          mapped.keys.toList(1)
      }
    }
    Integer.parseInt(groupedBySize.map(_.toString).mkString, 2)
  }

  def getLeastCommonBits(lines: List[String]): Int = {
    val columns = (0 until lines(0).length).map { charIndex =>
      val charsForLine = (0 until lines.length).map { i =>
        lines(i).charAt(charIndex)
      }
      charsForLine
    }

    val groupedBySize = columns.map {
      column => {
        val mapped = column.groupBy(identity).view.mapValues(_.size).toMap
        if (mapped.values.toList(0) < mapped.values.toList(1))
          mapped.keys.toList(0)
        else
          mapped.keys.toList(1)
      }
    }
    Integer.parseInt(groupedBySize.map(_.toString).mkString, 2)

  }

  def getOxygenGenerator(lines: List[String], colIndex: Int): Int = {
    val mostCommonBit = getMostCommonBitOnPosition(lines, colIndex, true)
    val validLines = lines.filter(_.charAt(colIndex) == mostCommonBit)
    if (validLines.length != 1) {
      getOxygenGenerator(validLines, colIndex + 1)
    }else{
      Integer.parseInt(validLines(0), 2)
    }
  }

  def getCO2Generator(lines: List[String], colIndex: Int): Int = {
    val leastCommonBit = getMostCommonBitOnPosition(lines, colIndex, false)
    val validLines = lines.filter(_.charAt(colIndex) == leastCommonBit)
    if (validLines.length != 1) {
      getCO2Generator(validLines, colIndex + 1)
    }else{
      Integer.parseInt(validLines(0), 2)
    }
  }




  def getMostCommonBitOnPosition(lines: List[String], position: Int, most: Boolean) = {
    val columns = (0 until lines(0).length).map { charIndex =>
      val charsForLine = (0 until lines.length).map { i =>
        lines(i).charAt(charIndex)
      }
      charsForLine
    }

    val groupedBySize = columns.map {
      column => {
        val mapped = column.groupBy(identity).view.mapValues(_.size).toMap
        if (mapped.size != 1){
          val moreCommonBit = if (most){
            if (mapped.values.toList(0) > mapped.values.toList(1)) {
              mapped.keys.toList(0)
            } else if (mapped.values.toList(0) == mapped.values.toList(1)) {
              '1'
            } else {
              mapped.keys.toList(1)
            }
          }else{
            if (mapped.values.toList(0) < mapped.values.toList(1)) {
              mapped.keys.toList(0)
            } else if (mapped.values.toList(0) == mapped.values.toList(1)) {
              '0'
            } else {
              mapped.keys.toList(1)
            }
          }

          moreCommonBit
        }else{
          mapped.keys.toList(0)
        }
      }
    }
    groupedBySize(position)
  }

  def hasBitOnPosition(string: String, bit: Char, position: Int): Boolean = string.lift(position).map(_ == bit).getOrElse(false)

  def getPart1(lines: List[String]) = {
    getMostCommonBits(lines) * getLeastCommonBits(lines)
  }

  def getPart2(lines: List[String]) = {
    getOxygenGenerator(lines, 0) * getCO2Generator(lines, 0)
  }

  val lines = Util.getFileLines(3)
  println(getPart1(lines))
  println(getPart2(lines))
}
