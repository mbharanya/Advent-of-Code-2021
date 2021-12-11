package ch.bharanya

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day11 extends App {
  type CoordMap = Map[Coordinate, Int]

  case class Coordinate(x: Int, y: Int) {
    def neighbors = {
      (for {
        newX <- (x - 1) to (x + 1)
        newY <- (y - 1) to (y + 1)
      } yield Coordinate(newX, newY)).toSet - this
    }
  }

  def findFlashes(map: CoordMap, flashes: Set[Coordinate]): Set[Coordinate] = {
    val next: Set[Coordinate] = flashes ++
      flashes
        .flatMap(_.neighbors)
        .filter(candidate => map
          .lift(candidate)
          .exists(_ + candidate.neighbors.intersect(flashes).size > 9))
    if (next == flashes) flashes else findFlashes(map, next)
  }


  def step(state: CoordMap): CoordMap = {
    val incremented = state.view.mapValues(_ + 1).toMap
    val flashes = findFlashes(incremented, incremented.filter(_._2 > 9).keySet)
    incremented.map {
      case (coord, value) => (coord,
        if (flashes.contains(coord)) 0
        else value + coord.neighbors.count(flashes.contains))
    }
  }

  def part1(input: List[String]) = {
    val map: CoordMap = (for {
      x <- (0 until input(0).size)
      y <- (0 until input.size)
    } yield Coordinate(x,y) -> input(y).split("")(x).toInt).toMap

    val stepResults: LazyList[CoordMap] = LazyList.iterate(map)(step)

    println(stepResults.take(101).map(_.values.count(_ == 0)).sum)
    stepResults
  }

  def part2(input: List[String]) = {
    val map: CoordMap = (for {
      x <- (0 until input(0).size)
      y <- (0 until input.size)
    } yield Coordinate(x,y) -> input(y).split("")(x).toInt).toMap

    val stepResults: LazyList[CoordMap] = LazyList.iterate(map)(step)

    val indexWithAll0 =  stepResults
      .zipWithIndex
      .filter(
        (l, i) => l.values.count(_ == 0) == map.size
      ).head._2

    println(
      indexWithAll0
    )
    indexWithAll0
  }

  part1(Util.getFileLines(11))
  part2(Util.getFileLines(11))


}