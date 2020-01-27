import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues._
import org.scalatest.wordspec.AsyncWordSpec

import moe.roselia.lisa

class MatchArgumentTests extends AsyncWordSpec with Matchers with ExpressionHelper {
  "matchArgument" should {
    import lisa.Evaluator._
    import lisa.LispExp._

    "match simple arguments" in {
      val pattern = "a".asSymbol :: "b".asSymbol :: Nil
      val arguments: List[Expression] = 1.asLisa :: 2.asLisa :: Nil
      val matchResult = matchArgument(pattern, arguments).value
      val expected = pattern.map(_.value).zip(arguments).toMap
      matchResult should have size 2
      matchResult shouldEqual expected
    }

    "handle empty on va-arg" in {
      val pattern = LisaList("...".asSymbol :: "ls".asSymbol :: Nil) :: Nil
      matchArgument(pattern, Nil).value shouldEqual Map("ls" -> LisaList.empty)
      matchArgument(LisaList(pattern) :: Nil, Nil) shouldBe empty
    }
  }

}
