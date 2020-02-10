import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa
import moe.roselia.lisa.Environments.{EmptyEnv, MutableEnv}
import moe.roselia.lisa.LispExp.{LisaList, LisaMapRecord}
import moe.roselia.lisa.Logical.{LogicalContext, LogicalRule}

class LogicalModuleTests extends AsyncWordSpec with Matchers with OptionValues with ExpressionHelper {
  import lisa.Logical.Queries
  object Queries extends Queries
  "unifyMatch" should {
    "match Lists" in {
      /**
       * (x x) match ((1 y 3) (z 2 3))
       *
       * x = (1 2 3)
       * y = 2
       * z = 1
      **/
      val pattern1 = LisaList("x".asSymbol :: "x".asSymbol :: Nil)
      val pattern2 = LisaList.from(
        LisaList.fromExpression(1, "y".asSymbol, 3),
        LisaList.fromExpression("z".asSymbol, 2, 3)
      )

      val matchResult = Queries.unifyMatch(pattern1, pattern2, EmptyEnv.newMutableFrame).value
      matchResult.getValueOption("x").value shouldBe LisaList.fromExpression(1, 2, 3)
      matchResult.getValueOption("y").value shouldBe 2.asLisa
      matchResult.getValueOption("z").value shouldBe 1.asLisa
    }

    "match varargs" in {
      val pattern1 = LisaList.fromExpression(Symbol("x"), LisaList.fromExpression("...".asSymbol, "y".asSymbol))
      val pattern2 = LisaList.fromExpression(1, 2, 3)

      val matchResult = Queries.unifyMatch(pattern1, pattern2, MutableEnv.createEmpty).value

      matchResult.getValueOption("x").value shouldBe 1.asLisa
      matchResult.getValueOption("y").value shouldBe LisaList.fromExpression(2, 3)
    }

    "deal with unknown values" in  {
      val pattern1 = LisaList("x".asSymbol :: "x".asSymbol :: Nil)
      val pattern2 = LisaList.from(
        LisaList.fromExpression("y".asSymbol, 1, "w".asSymbol),
        LisaList.fromExpression(2, "v".asSymbol, "z".asSymbol)
      )

      val matchResult = Queries.unifyMatch(pattern1, pattern2, EmptyEnv.newMutableFrame).value
      matchResult.getValueOption("y").value shouldBe 2.asLisa
      matchResult.getValueOption("v").value shouldBe 1.asLisa
      matchResult.getValueOption("w").value shouldBe "z".asSymbol
      matchResult.getValueOption("x") should contain oneOf(
          LisaList.fromExpression(2, 1, "w".asSymbol),
          LisaList.fromExpression(2, 1, "z".asSymbol))
    }

    "refuse to solve a fix-point equation" in {
      val pattern1 = LisaList("x".asSymbol :: "x".asSymbol :: Nil)
      val pattern2 = LisaList.from(
        "y".asSymbol,
        LisaList.fromExpression(1, LisaList.fromExpression("...".asSymbol, "y".asSymbol))
      )

      Queries.unifyMatch(pattern1, pattern2, MutableEnv.createEmpty) shouldBe empty
    }
  }

  "expression matcher" should {
    val context = LogicalContext(LisaList.fromExpression(
      LisaList.fromExpression("programmer".asSymbol, "linus".asAtom),
      LisaList.fromExpression("love".asSymbol, "YJSNPI".asAtom, "TON".asAtom),
      LisaList.fromExpression("love".asSymbol, "MUR".asAtom, "KMR".asAtom)
    ), Map.empty)
    "match expressions" in {
      val matcher = Queries.Matcher.fromExpression(LisaList.fromExpression("programmer".asSymbol, "who".asSymbol))
      val matchResult = matcher(LazyList(MutableEnv.createEmpty), context)
      matchResult should have size 1
      matchResult.head.getValueOption("who").value shouldBe "linus".asAtom
    }
    "handle atom case" in {
      val matcher = Queries.Matcher.fromExpression(LisaList.fromExpression("love".asSymbol, "x".asSymbol, "KMR".asAtom))
      val matchResult = matcher(LazyList(MutableEnv.createEmpty), context)
      matchResult should have size 1
      matchResult.head.getValueOption("x").value shouldBe "MUR".asAtom
    }
    "match multiple results if they exist" in {
      val matcher = Queries.Matcher.fromExpression(LisaList.fromExpression("love".asSymbol, "who".asSymbol, "for".asSymbol))
      val matchResult = matcher(LazyList(MutableEnv.createEmpty), context)
      matchResult should have size 2
    }
  }

  "rule matcher" should {
    val context = LogicalContext(LisaList.fromExpression(
      LisaList.fromExpression("programmer".asSymbol, "linus".asAtom),
      LisaList.fromExpression("love".asSymbol, "YJSNPI".asAtom, "TON".asAtom),
      LisaList.fromExpression("love".asSymbol, "TNOK".asAtom, "TNOK".asAtom),
      LisaList.fromExpression("love".asSymbol, "MUR".asAtom, "KMR".asAtom)
    ), Map.empty).addedRule("be-loved", LogicalRule(List(
      List("for", "who").map(_.asSymbol) -> LisaList.fromExpression("love".asSymbol, "who".asSymbol, "for".asSymbol)
    ))).addedRule("self-loved", LogicalRule(List(
      List("who".asSymbol) -> LisaList.fromExpression("love".asSymbol, "who".asSymbol, "who".asSymbol)
    )))

    "apply rules correctly" in {
      val matcher = Queries.Matcher.fromRule(context.getRule("be-loved").get,
        List("KMR".asAtom, "who".asSymbol), EmptyEnv)
      val matchResult = matcher(LazyList(MutableEnv.createEmpty), context)
      matchResult should have length 1
      matchResult.head.getValueOption("who").value shouldBe "MUR".asAtom
    }

    "treat same variables" in {
      val matcher = Queries.Matcher.fromRule(context.getRule("self-loved").get,
        List("who".asSymbol), EmptyEnv)
      val matchResult = matcher(LazyList(MutableEnv.createEmpty), context)
      matchResult should have length 1
      matchResult.head.getValueOption("who").value shouldBe "TNOK".asAtom
    }
  }
}
