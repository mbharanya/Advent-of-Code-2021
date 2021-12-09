package ch.bharanya

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

    def getBasinSizeofPoint(x: Int, y: Int, alreadyCounted: List[(Int, Int)]): List[(Int, Int)] = {
      val newCounted: List[(Int, Int)] = (x, y) :: alreadyCounted

      if (!map.lift(x, y).exists(_ == 9) && !alreadyCounted.contains((x, y))) {
        val up = (x, y - 1) -> map.lift(x, y - 1)
        val down = (x, y + 1) -> map.lift(x, y + 1)
        val left = (x - 1, y) -> map.lift(x - 1, y)
        val right = (x + 1, y) -> map.lift(x + 1, y)


//        val validOthers = List(up, down, left, right).filter(_ match {
//          case (newX, newY) -> value => value.exists(_ < 9)
//        })

        val upElements = getBasinSizeofPoint(up._1._1, up._1._2, newCounted)
        val downElements = getBasinSizeofPoint(down._1._1, down._1._2, upElements)
        val leftElements = getBasinSizeofPoint(left._1._1, left._1._2, downElements)
        val rightElements = getBasinSizeofPoint(right._1._1, right._1._2, leftElements)


        rightElements
        //        validOthers.flatMap(_ match {
        //          case (newX, newY) -> value => if (!newCounted.contains(newX, newY)) getBasinSizeofPoint(newX, newY, newCounted) else newCounted
        //        }).toSet.toList


      } else {
        //        println(s"basin size of ${(x, y)}->${map.lift(x, y)} = ${alreadyCounted.size}")
        alreadyCounted
      }
    }
  }

  def getLowestPoints(maze: Maze): CoordMap = {
    maze.map.filter {
      _ match {
        case ((x, y), value) => {
          val adjacents = maze.getAdjacent(x, y)
          value < adjacents.flatten.min
        }
      }
    }
  }

  def getRiskLevel(maze: Maze) = getLowestPoints(maze).map(_ match { case ((x, y), value) => 1 + value }).sum

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

    val lowest = getLowestPoints(maze)
    val basinSizes = lowest.map(_ match { case ((x, y), _) => {
      println(s"testing  lowest ${(x, y)}")
      maze.getBasinSizeofPoint(x, y, Nil).toSet.size
    }
    }).toList.sorted.reverse.slice(0, 3)

    println(basinSizes.product)
  }

  part1()
  //  part2()
}
