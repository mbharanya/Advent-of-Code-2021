package ch.bharanya

import scala.io.Source

object Day2 extends App {

  def getMultipliedPositionDepth(instructions: List[String]) = {
    def recursive(i: Int, horizontalPosition: Int, depth: Int): (Int, Int) = {
      instructions.lift(i) match {
        case Some(instruction) =>
          instruction.split(" ") match {
            case Array(direction, amount) => direction match {
              case "forward" => recursive(i + 1, horizontalPosition + amount.toInt, depth)
              case "down" => recursive(i + 1, horizontalPosition, depth + amount.toInt)
              case "up" => recursive(i + 1, horizontalPosition, depth - amount.toInt)
            }
          }
        case None => (horizontalPosition, depth)
      }
    }

    val (horizontalPosition, depth) = recursive(0, 0, 0)

    horizontalPosition * depth
  }

  def getWithAim(instructions: List[String]) = {

    def recursive(i: Int, horizontalPosition: Int, depth: Int, aim: Int): (Int, Int) = {
      instructions.lift(i) match {
        case Some(instruction) =>
          instruction.split(" ") match {
            case Array(direction, amount) => direction match {
              case "forward" => recursive(i + 1, horizontalPosition + amount.toInt, depth + (amount.toInt * aim), aim)
              case "down" => recursive(i + 1, horizontalPosition, depth, aim + amount.toInt )
              case "up" => recursive(i + 1, horizontalPosition, depth, aim - amount.toInt )
            }
          }
        case None => (horizontalPosition, depth)
      }
    }

    val (horizontalPosition, depth) = recursive(0, 0, 0, 0)

    horizontalPosition * depth
  }

  val filename = "res/day2.txt"

  val data = Source.fromFile(filename).getLines.toList
  println(getMultipliedPositionDepth(data))

  println(getWithAim(data))

}
