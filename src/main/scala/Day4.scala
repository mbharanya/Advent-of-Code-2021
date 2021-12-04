package ch.bharanya

object Day4 extends App {
  case class BoardPosition(x: Int, y: Int, value: Int)

  class Board(boardElements: List[BoardPosition]) {

    def hasWinningColumnOrRow(winningNumbers: List[Int]) = {
      (0 until 5 ).exists(i => {
        val xValues = boardElements.filter(_.x == i)
        val yValues = boardElements.filter(_.y == i)
        val hasWinningX = xValues.map(_.value).forall(winningNumbers.contains(_))
        val hasWinningY = yValues.map(_.value).forall(winningNumbers.contains(_))
        hasWinningX || hasWinningY
      })
    }

    def containsAllValues(a: List[Int], b: List[Int]) = {
      a.forall(b.contains(_))
    }

    def getWinScore(winningNumbers: List[Int]): Int = boardElements.map(_.value).filter(!winningNumbers.contains(_)).sum * winningNumbers(winningNumbers.length - 1)
  }

  def createBoards(boards: List[String]): List[Board] = {
    val boardClusters = boards
      .mkString("\n")
      .split("\n\n")

    boardClusters.map { boardCluster =>
      val boardLine = boardCluster.split("\n").toList.zipWithIndex
      val boardPositions = for {
        (lineValues, y) <- boardLine
        (value, x) <- lineValues.trim.replaceAll("  ", " ").split(" ").toList.zipWithIndex
//        _ = println(s"creating board pos with ${x}, ${y}, ${value}")
      } yield BoardPosition(x, y, value.toInt)
      new Board(boardPositions.toList)
    }.toList
  }

  def checkWinningNumbers(winningNumbers: List[Int], boards: List[Board]): List[(Int, List[Board], List[Int])] = {
    (0 until winningNumbers.length).reverse.map(i => {
      (i, boards.filter(board => board.hasWinningColumnOrRow(winningNumbers.dropRight(i))), winningNumbers.dropRight(i))
    }).filter((pos, list, _) => list.length > 0).toList
  }

  def part1(list: List[String]) = {
    val winningNumbers = list.head.split(",")
    val boards = Day4.createBoards(list.tail.tail)

    val winners = Day4.checkWinningNumbers(winningNumbers.map(_.toInt).toList, boards)
    val result = winners.head match {
      case (i, board, winNumbers) => board.head.getWinScore(winNumbers)
    }
    println(result)
  }

  def part2(list: List[String]) = {
    val winningNumbers = list.head.split(",")
    val boards = Day4.createBoards(list.tail.tail)

    val winners = Day4.checkWinningNumbers(winningNumbers.map(_.toInt).toList, boards)
    val validBoards = winners.filter((_, b, _) => b.length == boards.length -1 || b.length == boards.length)

    val boardsBeforeLast: (List[Board], List[Int]) = validBoards(7).match {
      case (_, boards, winNums) => (boards, winNums)
    }

    val boardsAfterLast: (List[Board], List[Int]) = validBoards(8).match {
      case (_, boards, winNums) => (boards, winNums)
    }


    val last = boardsAfterLast match {
      case (boardsAfter, winNums) => (boardsAfter.filter(!boardsBeforeLast._1.contains(_)), winNums)
    }

    val result = last._1.head.getWinScore(last._2)
    println(result)
    result
  }

  part1(Util.getFileLines(4))
  part2(Util.getFileLines(4))
}
