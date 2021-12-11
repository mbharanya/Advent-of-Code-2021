package ch.bharanya

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day11 extends App {
  type CoordMap = Map[(Int, Int), Int]

  class Octopus(val x: Int, val y: Int, val maybeEnergy: Option[Int], val hasFlashed: Boolean) {
    def tick(map: CoordMap): List[Octopus] = {
      val newEnergy = maybeEnergy.map(_ + 1)
      if (newEnergy.exists(_ > 9) && !this.hasFlashed) {
        val nextToTick = for {
          newX <- (x - 1) until (x + 1)
          newY <- (y - 1) until (y + 1)
        } yield new Octopus(newX, newY, map.lift(newX, newY), false)
        new Octopus(x, y, newEnergy, true) +: nextToTick.filter(!_.isSame(this)).toList //add self in flashed state
      } else {
        List(new Octopus(x, y, newEnergy, false))
      }
    }

    def isSame(a: Octopus) = a.x == this.x && a.y == this.y
  }

  class OctopusMap(map: CoordMap) {
    def tickOctopi(queue: Queue[Octopus], alreadyCounted: List[Octopus]): List[Octopus] = {
      val (octopus, newQueue) = queue.dequeue

      val newValuesToQueue = octopus.tick(map).filter(_ match {
        case octopus: Octopus => octopus.maybeEnergy.exists(_ > 0) && !alreadyCounted.contains(octopus)
      })

      val newNewQueue = newQueue.enqueueAll(newValuesToQueue)

      if (!newNewQueue.isEmpty) {
        tickOctopi(newNewQueue, newValuesToQueue ::: alreadyCounted)
      } else {
        alreadyCounted
      }
    }

  }
}