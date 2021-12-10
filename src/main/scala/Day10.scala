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

  val completionValueMap = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )


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

  def getCompletions(charsInLine: List[Char]): List[Char] = iterate(charsInLine, Nil) match {
    case Valid => Nil
    case Invalid(expected, actual) => Nil
    case Incomplete(remaining) => remaining
  }

  def getValueOfCompletionString(completionList: List[Char]) = {
    completionList.foldLeft(0L)((acc, char) => (acc * 5) + completionValueMap(char))
  }

  def median(list: List[Long]) = list.sortWith(_ < _).drop(list.length / 2).head

  def part1(lines: List[String]) = lines.map(l => {
    iterate(l.toCharArray.toList, Nil) match {
      case Valid => 0
      case Invalid(expected, actual) => valueMap(actual)
      case Incomplete(_) => 0
    }
  }).sum

  def part2(lines: List[String]) = median(lines.map(
    l => getCompletions(l.toCharArray.toList)
  ).map(getValueOfCompletionString(_)).filter(_ > 0))

  println(part1(Util.getFileLines(10)))
    println(part2(Util.getFileLines(10)))
}
