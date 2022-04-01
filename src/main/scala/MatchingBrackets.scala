object MatchingBrackets extends App{

  val matching = Map(
    '[' -> ']',
    '(' -> ')',
    '{' -> '}',
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    )

  def check(s: String): Boolean ={
    val allowedBrackets = List('(',')','[',']','{','}')
    val brackets = s.toList.filter(allowedBrackets.contains)
    clearInside(brackets)
  }

  def clearInside(brackets: List[Char]): Boolean = brackets match {
    case Nil => true
    case m :: Nil => false
    case _ => {
      val updatedBrackets = brackets.foldLeft(List.empty[Char]){(result,n) =>
        if(result.isEmpty) List(n)
        else if(matching(result.last)== n ) result.init
        else n +: result
      }
      clearInside(updatedBrackets)
    }
  }

  println(check("{ { } [ ] [ [ [ ] ] ] }"))
}
