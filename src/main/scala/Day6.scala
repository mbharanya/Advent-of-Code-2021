package ch.bharanya

import scala.annotation.tailrec

object Day6 extends App {
  class LanternFish(val timer: Long) {
    def getDecreasedTimerFish() : List[LanternFish] = {
      val newTimer = timer - 1
      if (newTimer < 0) List(new LanternFish(6), new LanternFish(8)) else List(new LanternFish(timer - 1))
    }
  }

  @tailrec
  def tick(state: List[Long], day: Long, stopDay: Long): List[Long] = {
    val fishInCurrentState = for{
      stateEntry <- state
      asFish = new LanternFish(stateEntry)
      newFish <- asFish.getDecreasedTimerFish()
    } yield newFish.timer
    //    println(s"Day ${day}, ${fishInCurrentState}")
    if (day != stopDay){
      println(day)
      println(s"Current fish: ${fishInCurrentState.length}")
      tick(fishInCurrentState, day + 1, stopDay)
    }else{
      fishInCurrentState
    }
  }

//  def tickFast()

  def part1(): Unit ={
    val state = Util.getSplitLine(6, ",").map(_.toLong)
    val res = tick(state, 1, 80)
    println(res.length)
  }

  def part2(): Unit ={
    val state = Util.getSplitLine(6, ",").map(_.toLong)
    val res = tick(state, 1, 256)
    println(res.length)
  }

  //  part1()
//  part2()
}
