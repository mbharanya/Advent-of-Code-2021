package ch.bharanya

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day9 extends App {
  type CoordMap = Map[(Int, Int), Int]

  class Maze(val map: CoordMap) {
    def getAdjacent(x: Int, y: Int): List[Option[Int]] = {
      val up = map.lift(x, y - 1)
      val down = map.lift(x, y + 1)
      val left = map.lift(x - 1, y)
      val right = map.lift(x + 1, y)

      List(up, down, left, right)
    }

    def getLowestPoints(): CoordMap = {
      map.filter {
        _ match {
          case ((x, y), value) => {
            val adjacents = getAdjacent(x, y)
            value < adjacents.flatten.min
          }
        }
      }
    }

    def getBasin2(queue: Queue[(Int, Int)], alreadyCounted: List[(Int, Int)]): List[(Int, Int)] = {
      val ((x: Int, y: Int), newQueue) = queue.dequeue

      val up = (x, y - 1) -> map.lift(x, y - 1)
      val down = (x, y + 1) -> map.lift(x, y + 1)
      val left = (x - 1, y) -> map.lift(x - 1, y)
      val right = (x + 1, y) -> map.lift(x + 1, y)

      val newValuesToQueue = List(up, down, left, right).filter(_ match {
        case (newX, newY) -> value => value.exists(_ < 9) && !alreadyCounted.contains((newX, newY))
      }).map(_._1)

      val newNewQueue = newQueue.enqueueAll(newValuesToQueue)

      if (!newNewQueue.isEmpty) {
        getBasin2(newNewQueue, newValuesToQueue ::: alreadyCounted)
      } else {
        alreadyCounted
      }
    }
  }


  def getRiskLevel(maze: Maze) = maze.getLowestPoints().map(_ match { case ((x, y), value) => 1 + value }).sum

  def part1() = {
    val lines = Util.getFileLines(9)
    val map = for {
      x <- (0 until lines(0).size)
      y <- (0 until lines.size)
    } yield (x, y) -> lines(y).split("")(x).toInt

    println(getRiskLevel(new Maze(map.toMap)))
  }

  def part2() = {
    val lines = Util.getFileLines(9)
    val map = for {
      x <- (0 until lines(0).size)
      y <- (0 until lines.size)
    } yield (x, y) -> lines(y).split("")(x).toInt

    val maze = new Maze(map.toMap)

    val lowest = maze.getLowestPoints()
    val basins = lowest.keys.toList.map {
      (x, y) =>
        maze.getBasin2(
          Queue.from(List((x, y))), List((x, y))
        )
    }

    val sorted = basins.map(_.size).toList.sorted
    val basinSizes = basins.map(_.size).toList.sorted.reverse.slice(0, 3)
    println(basinSizes.product)
  }

  part1()
  part2()
}
