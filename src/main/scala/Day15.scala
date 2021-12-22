package ch.bharanya

import scala.collection.immutable.Queue
import scala.collection.mutable

object Day15 extends App {
  type CoordMap = Map[Coordinate, Int]

  case class CoordinateDistance(coordinate: Coordinate, var distance: Int) // ewwww mutability https://stackoverflow.com/questions/9103742/change-priority-of-items-in-a-priority-queue

  def MyOrdering = new Ordering[CoordinateDistance] {
    def compare(a : CoordinateDistance, b : CoordinateDistance) = a.distance.compare(b.distance)
  }

  case class Coordinate(x: Int, y: Int) {
    def verticalAndHorizontalNeighbors = List(
      Coordinate(x - 1, y),
      Coordinate(x + 1, y),
      Coordinate(x, y - 1),
      Coordinate(x, y + 1)
    )
  }

  case class CoordinateCounter(coordinate: Coordinate, counter: Int)

  def getMap(input: List[String]): CoordMap = {
    (
      for {
        y <- 0 until input.length
        x <- 0 until input(0).length
      } yield Coordinate(x, y) -> input(y).split("")(x).toInt
      ).toMap
  }


  def getInitialPrioQueue(start: Coordinate, map: CoordMap) = {
    val priorityQueue = mutable.PriorityQueue[CoordinateDistance]()(MyOrdering.reverse)

    // set all except start value to infinite distance
    val distanceMap = map.map((coordinate, value) => {
      if (coordinate == start) {
        priorityQueue.enqueue(CoordinateDistance(coordinate, 0))
      } else {
        priorityQueue.enqueue(CoordinateDistance(coordinate, Integer.MAX_VALUE))
      }
    })

    priorityQueue
  }

  def setQueueValuesForNeighbors(start: Coordinate, priorityQueue: mutable.PriorityQueue[CoordinateDistance], map: CoordMap): mutable.PriorityQueue[CoordinateDistance] = {
    val neighbors = start.verticalAndHorizontalNeighbors
    neighbors.foreach(neighborCoord => {
      for {
        coordValue <- map.lift(neighborCoord)
      } yield priorityQueue.find(p => p.coordinate == neighborCoord) match {
        case Some(coordDist: CoordinateDistance) => coordDist.distance = coordValue
        case None => println("not found")
      }
    })
    // clone to reorder
    priorityQueue.clone()
  }


  def part1(input: List[String], lastCoordinate: Coordinate) = {
    val map = getMap(input)



  }

}
