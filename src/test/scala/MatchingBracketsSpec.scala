import MatchingBrackets.check
import org.scalatest._
import flatspec._
import matchers._


class MatchingBracketsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "A MatchingBrackets.check() "

  it should "return true/false if brackets are matching " in {
    check("{ { } [ ] [ [ [ ] ] ] }") shouldBe  true
    check("{ { } [ ] [ [ [ ] ] ]") shouldBe  false
  }

}