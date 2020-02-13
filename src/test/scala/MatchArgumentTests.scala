import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues._
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa
import moe.roselia.lisa.Preludes

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

    "match quoted symbols" in {
      val pattern = List(Quote("a".asSymbol))
      matchArgument(pattern, "a".asSymbol :: Nil).value shouldBe empty
    }

    "match symbols" in {
      val pattern = List("a".asSymbol, "b".asSymbol)
      val expected = pattern.map(_.value).zip(pattern).toMap
      matchArgument(pattern, pattern).value shouldBe expected
    }

    "handle guard expressions" in {
      val pattern = List(
        "a".asSymbol, "b".asSymbol,
        LisaList.fromExpression("when".asSymbol,
          LisaList.fromExpression(">".asSymbol, "a".asSymbol, "b".asSymbol))
      )
      val data1 = List[Expression](1, 2)
      val data2 = List[Expression](2, 1)
      val data2Expected = pattern.collect {
        case Symbol(s) => s
      }.zip(data2).toMap

      matchArgument(pattern, data1, inEnv = Preludes.preludeEnvironment) shouldBe empty
      matchArgument(pattern, data2, inEnv = Preludes.preludeEnvironment).value shouldBe data2Expected
    }
  }

}
