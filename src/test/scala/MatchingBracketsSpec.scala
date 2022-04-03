import MatchingBrackets.check
import org.scalatest._
import flatspec._
import matchers._


class MatchingBracketsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "A MatchingBrackets.check() "

  it should "return true if brackets are matching " in {
    check("{[()]}") shouldBe  true
    check("{}[]()") shouldBe  true
    check("[{ { } [ ] [ ]} [ ] ]") shouldBe  true
  }

  it should "return false if brackets are not matching " in {
    check("{{{}") shouldBe false
    check("[{ { } [ ] {[ }]} [ ] ]") shouldBe false
    check("{]") shouldBe false
  }
}