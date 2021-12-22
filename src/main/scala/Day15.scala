package ch.bharanya

import scala.collection.immutable.Queue

object Day15 extends App {
  type CoordMap = Map[Coordinate, Int]

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


  def findAllPaths(queue: Queue[CoordinateCounter], foundPath: List[CoordinateCounter], coordMap: CoordMap): List[CoordinateCounter] = {
    if (!queue.isEmpty) {
      val (currentElement, newQueue) = queue.dequeue
      val newVisited = currentElement :: foundPath

      if (currentElement.coordinate != Coordinate(0,0)) {

        val newones = currentElement.coordinate.verticalAndHorizontalNeighbors
          .map(neighbor => CoordinateCounter(neighbor, currentElement.counter + 1))
          .filter(c => coordMap.lift(c.coordinate).isDefined)

        val newCounters = newones.filter(c => {
          !newVisited.map(_.coordinate).contains(c.coordinate)
        })
          .filter(c => {
//            coordMap.lift(c.coordinate).isDefined
            (for {
              neighborValue <- coordMap.lift(c.coordinate)
              currentElementValue <- coordMap.lift(currentElement.coordinate)
            } yield neighborValue <= currentElementValue).getOrElse(false) //TODO: false?
          })

//        val newCounters = currentElement.coordinate.verticalAndHorizontalNeighbors
//          .map(
//            neighbor => CoordinateCounter(neighbor, currentElement.counter + 1)
//          )
//          //If the cell is a wall, remove it from the list
//          .filter(c => {
//            coordMap.lift(c.coordinate).isDefined
////            (for {
////              neighborValue <- coordMap.lift(c.coordinate)
////              currentElementValue <- coordMap.lift(currentElement.coordinate)
////            } yield neighborValue <= currentElementValue).getOrElse(false) //TODO: false?
//          })
//          // If there is an element in the main list with the same coordinate, remove it from the cells list
//          .filter(c => {
//            !newVisited.exists(_.coordinate == currentElement.coordinate)
//          })
        findAllPaths(newQueue.enqueueAll(newCounters), newVisited, coordMap)
      }else {
        foundPath
      }
    } else {
      foundPath
    }
  }

  def part1(input: List[String], lastCoordinate: Coordinate) = {
    val map = getMap(input)
    findAllPaths(
      Queue.from(List(new CoordinateCounter(lastCoordinate, 0))),
      Nil,
      map
    )
  }

}
