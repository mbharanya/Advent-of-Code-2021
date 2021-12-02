package ch.bharanya

import scala.io.Source

object Day2 extends App{

  def getMultipliedPositionDepth(instructions: List[String]) = {
    var horizontalPosition = 0
    var depth = 0
    var aim = 0
    instructions.map(instruction => {
      instruction.split(" ") match {
        case Array(direction, amount) => direction match{
          case "forward" => horizontalPosition = horizontalPosition + amount.toInt
          case "down" => depth = depth + amount.toInt
          case "up" => depth = depth - amount.toInt
        }
      }
    })
    horizontalPosition * depth
  }

  def getWithAim(instructions: List[String]) = {
    var horizontalPosition = 0
    var depth = 0
    var aim = 0
    instructions.map(instruction => {
      instruction.split(" ") match {
        case Array(direction, amount) => direction match{
          case "forward" => {
            horizontalPosition = (horizontalPosition + amount.toInt)
            depth = depth + (amount.toInt * aim)
          }
          case "down" => {
            aim = aim + amount.toInt
          }
          case "up" =>  {
            aim = aim - amount.toInt
          }
        }
      }
    })
    horizontalPosition * depth
  }
  val filename = "res/day2.txt"

  val data = Source.fromFile(filename).getLines.toList
  println(getMultipliedPositionDepth(data))

  println(getWithAim(data))

}
