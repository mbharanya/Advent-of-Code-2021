package ch.bharanya

object Day12 extends App {

  sealed trait Cave()

  class StartCave extends Cave
  class EndCave extends Cave

  case class SmallCave(letters: String) extends Cave
  case class LargeCave(letters: String) extends Cave

  type CaveConnections = Map[Cave,List[Cave]]

  def getConnections(connectionLines: List[String]): CaveConnections = {
    connectionLines.flatMap(l => {
      val fromCave = getCaveType(l.split("-")(0))
      val toCave = getCaveType(l.split("-")(1))
      List(
        fromCave -> toCave,
        toCave -> fromCave
      )
    }).toMap
    ???
  }


  def getCaveType(caveStr: String): Cave = {
    if (caveStr == "start") {
      new StartCave
    } else if (caveStr == "end") {
      new EndCave
    } else if (caveStr.toLowerCase == caveStr) {
      SmallCave(caveStr)
    } else {
      LargeCave(caveStr)
    }
  }


  def step(currentCave: Cave, nextCave: Cave, visited: List[Cave]) = {
    val newVisited = currentCave match {
      case _: ch.bharanya.Day12.StartCave => currentCave +: visited
      case smallCave: SmallCave => if (visited.contains(smallCave)) Nil else currentCave +: visited
      case _: ch.bharanya.Day12.LargeCave => currentCave +: visited
      case _: ch.bharanya.Day12.EndCave => ???
    }
  }

//  def part1(lines: List[String]) = {
//    val connections = getConnections(lines)
//    step(new StartCave,)
//  }

}
