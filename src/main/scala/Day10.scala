package ch.bharanya

object Day10 extends App {
  sealed trait Program

  case object Valid extends Program

  case class Invalid(expected: Char, actual: Char) extends Program

  case class Incomplete(missing: List[Char]) extends Program

  val bracketMap = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  val valueMap = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  //  class Line(val line: String){
  //    def isCorrupted() = false
  //    def isComplete() = {
  //      val freqMap = line
  //        .split("")
  //        .groupMapReduce(identity)(_ => 1L)(_ + _)
  //      freqMap
  //        .filter((char, _) => bracketMap.keySet.contains(char)) // only opening
  //        .forall((char, numOccurrence) => {
  //          (for {
  //            associatedBracket <- bracketMap.lift(char)
  //            frequencyOfAssociatedBracket <- freqMap.lift(associatedBracket)
  //            _ = println(s"${char}x${numOccurrence} is assoc with ${associatedBracket}x${frequencyOfAssociatedBracket}")
  //          } yield numOccurrence == frequencyOfAssociatedBracket).getOrElse(false)
  //        })
  //    }
  //
  //
  //
  //  }

  def iterate(charsInLine: List[Char], stack: List[Char]): Program = charsInLine match {
    case currentChar :: tail =>
      if (bracketMap.contains(currentChar)) {
        iterate(tail, currentChar +: stack)
      } else {
        stack match {
          case bracket :: stackTail => if (bracketMap.contains(bracket) && bracketMap(bracket) == currentChar) {
            iterate(tail, stackTail)
          } else {
            Invalid(bracketMap(bracket), currentChar)
          }
        }
      }
    case Nil if stack.nonEmpty => Incomplete(stack.map(bracketMap))
    case Nil => Valid
  }

  def part1(lines: List[String]) = lines.map(l => {
    iterate(l.toCharArray.toList, Nil) match {
      case Valid => 0
      case Invalid(expected, actual) => valueMap(actual)
      case Incomplete(_) => 0
    }
  }).sum

  println(part1(Util.getFileLines(10)))
}
