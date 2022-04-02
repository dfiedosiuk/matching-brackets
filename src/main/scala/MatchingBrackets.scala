object MatchingBrackets extends App {

  val matching = Map(
    '[' -> ']',
    '(' -> ')',
    '{' -> '}',
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    'p' -> 'p'
  )

  def check(s: String): Boolean = {
    val allowedBrackets = List('(', ')', '[', ']', '{', '}')
    val brackets = s.toList.filter(allowedBrackets.contains)
    clearInside(brackets)
  }

  def clearInside(brackets: List[Char]): Boolean = brackets match {
    case Nil => true
    case m :: Nil => false
    case _ => {
      val updatedBrackets = brackets.foldLeft(List.empty[Char]) { (result, n) =>
          if (result.nonEmpty && matching(result.last) == n) {
          val tmp = result.init :+ 'p'
          tmp :+ 'p'
        } else {
          result :+ n
        }
      }
      println(updatedBrackets.filterNot(List('p').contains))
      if (updatedBrackets == brackets) false
      else clearInside(updatedBrackets.filterNot(List('p').contains))
    }
  }

  val s = "{ { } [ ] [ [ [ ] ] ] }{{{"
  val z = "{ { } [ ] [ ]} [ ] "
  val x = "{[ ]}"
  println(check(s))
  println(check(z))
  println(check(x))
}
