package ch.bharanya

object Day3 extends App{
  def getMostCommonBits(lines: List[String]): Int = {
    val columns = (0 until lines(0).length).map { charIndex =>
      val charsForLine = (0 until lines.length).map { i =>
         lines(i).charAt(charIndex)
      }
      charsForLine
    }

    val groupedBySize = columns.map{
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

    val groupedBySize = columns.map{
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
  def getPart1(lines: List[String]) = {
    getMostCommonBits(lines) * getLeastCommonBits(lines)
  }

  val lines = Util.getFileLines(3)
  println(getPart1(lines))
}
