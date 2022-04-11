import scala.annotation.tailrec

object MatchingBrackets extends App {

  def check(s: String): Boolean = {
    val allowedBrackets = List('(', ')', '[', ']', '{', '}')
    val brackets = s.toList.filter(allowedBrackets.contains)
    clearInside(brackets)
  }

  @tailrec
  def clearInside(brackets: List[Char]): Boolean = {
    val matching = Map(
      '[' -> ']',
      '(' -> ')',
      '{' -> '}',
      ')' -> '(',
      ']' -> '[',
      '}' -> '{'
    )
    brackets match {
      case Nil => true
      case m :: Nil => false
      case _ =>
        val updatedBrackets = brackets.foldLeft(List.empty[Char]) { (result, n) =>
          if (result.nonEmpty && matching(result.last) == n) result.init
          else result :+ n
        }
        if (updatedBrackets == brackets) false
        else clearInside(updatedBrackets)
    }
  }
}

