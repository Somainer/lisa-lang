import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa
import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, MutableEnv}
import moe.roselia.lisa.LispExp.{Expression, LisaList, LisaMapRecord, LisaRecordWithMap, NilObj}
import moe.roselia.lisa.Logical.{LogicalContext, LogicalEnvironment, LogicalRule}

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
      val pattern2 = LisaList.fromExpression(
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

      Queries.unifyMatch(pattern2, pattern1, MutableEnv.createEmpty).value shouldBe matchResult
    }

    "deal with unknown values" in  {
      val pattern1 = LisaList("x".asSymbol :: "x".asSymbol :: Nil)
      val pattern2 = LisaList.fromExpression(
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
      val pattern2 = LisaList.fromExpression(
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

  "matchLogicalArgument" should {
    import LogicalRule.matchLogicalRuleArgument

    "match simple situation" in {
      val pattern = "a".asSymbol :: "b".asSymbol :: Nil
      val arguments: List[Expression] = 1.asLisa :: 2.asLisa :: Nil
      val (matched, introduced) = matchLogicalRuleArgument(pattern, arguments).value
      val expected = pattern.map(_.value).zip(arguments).toMap
      matched should have size 2
      matched shouldEqual expected
      introduced shouldBe empty
    }

    "handle introduced values" in {
      val pattern = "a".asAtom :: "b".asSymbol :: Nil
      val pattern2 = "a".asAtom :: "y".asSymbol :: Nil
      val arguments = "x".asSymbol :: "y".asSymbol :: Nil
      val (matched, introduced) = matchLogicalRuleArgument(pattern, arguments).value

      introduced should have size 1
      introduced("x") shouldEqual "a".asAtom
      matched("b") shouldBe "y".asSymbol

      val (matched2, introduced2) = matchLogicalRuleArgument(pattern2, arguments).value

      introduced2 should have size 1
      introduced2("x") shouldEqual "a".asAtom
      matched2("y") shouldBe "y".asSymbol
    }
  }

  "Query Module" should {
    val lcEnv = LogicalEnvironment(LogicalContext.empty)
    val env = lcEnv.implementationsEnvironment.combinedWithPrelude
    """
      |(define-macro (facts (... fs))
      |    (define fact-clauses (map fs &(list 'fact #)))
      |    '(let () ~~fact-clauses))
      |
      |(facts
      |    (path 0 1 1)
      |    (path 1 3 3)
      |    (path 3 2 2)
      |    (path 1 2 4))
      |
      |(define-rule (is-connected x y)
      |    (or (path x y _)
      |        (and (path x u _)
      |            (is-connected u y))))
      |
      |(define-rule (not-connected x y)
      |    (not (is-connected x y)))
      |
      |(define-rule (distance x y d)
      |    (or (path x y d)
      |        (and (path x u d1)
      |             (distance u y d2)
      |             (<- d (+ d1 d2)))))
      |(define-rule (single-source x)
      |    (and (path x _ _) (unique (path x y _))))
      |""".stripMargin.toListOfLisa.evalOn(env)

    def runQuery(qs: String) = {
      val lisaList = lisa"'$qs".evalOn(EmptyEnv)
      lcEnv.runMatch(lisaList, env)
    }

    "query for facts" in {
      runQuery("(path 0 1 1)") should not be empty
      runQuery("(path 0 1 2)") shouldBe empty
      runQuery("(path 1 x d)") should have size 2
    }

    "apply rules correctly" in {
      runQuery("(is-connected 0 2)") should have size 2
    }

    "query for right results" in {
      val result = runQuery("(distance 0 3 d)").head
      result.collectDefinedValues should have size 1
      result.getValueOption("d").value shouldBe 4.asLisa

      runQuery("(distance 0 3 4)") should not be empty
    }

    "handle not cases" in {
      runQuery("(not-connected 0 2)") shouldBe empty
    }

    "handle unique combinator" in {
      val output = runQuery("(single-source x)")
      output should have size 2
      output.map(_.get("x")) should contain allElementsOf LisaList.fromExpression(0, 3)
    }

    "dump and load correctly" in {
      val dumpedContext = lisa"(current-context)".evalOn(env)
      dumpedContext shouldBe a[LisaRecordWithMap[_]]
      val loadedContext = "(logical/push-context ctx) (current-context/raw)"
        .toListOfLisa
        .evalOn(
          CombineEnv(Seq(lisa.Logical.LogicalModuleEnvironment, EmptyEnv.withValue("ctx", dumpedContext)))
        ).last.asInstanceOf[lisa.LispExp.WrappedScalaObject[_]].obj
      loadedContext shouldEqual lcEnv.context
    }
  }
}
