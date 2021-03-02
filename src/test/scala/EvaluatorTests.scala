import moe.roselia.lisa.Environments.EmptyEnv
import moe.roselia.lisa.Evaluator.EvalSuccess
import org.scalatest.funsuite._
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers

import moe.roselia.lisa.{LispExp, _}

class EvaluatorTests extends AsyncFunSuite with Matchers with ExpressionHelper {

  import LispExp.Implicits._
  import LispExp._
  import util.chaining._

  test("Factorial function should work well") {
    val factZero = Evaluator.eval(define("fact".asSymbol, lambdaExpression(0 :: Nil, 1)), Preludes.preludeEnvironment)
    assert(factZero.isSuccess)
    val Evaluator.EvalSuccess(`NilObj`, env) = factZero
    assert(env.getValueOption("fact").get.isInstanceOf[Closure])
    val finallyFact = Evaluator.eval(
      defun(symbol"fact",
        List("n".asSymbol),
        "*".asSymbol.apply("n".asSymbol, "fact".asSymbol.apply("-".asSymbol.apply("n".asSymbol, 1)))),
      env
    )
    val EvalSuccess(`NilObj`, factEnv) = finallyFact
    val fact = factEnv.getValueOption("fact").get
    assert(fact.isInstanceOf[PolymorphicExpression])
    val EvalSuccess(num, _) = Evaluator.eval(fact(5), EmptyEnv)
    num shouldEqual (SInteger(120))
    a[StackOverflowError] should be thrownBy {
      Evaluator.eval(fact(10000), EmptyEnv)
    }
  }

  test("Tall call optimization should work well") {
    val factWithTailRec =
      """
        |(define (fact n)
        |   (define (f 0 x) x)
        |   (define (f n x) (f (- n 1) (* x n)))
        |   (f n 1))
        |""".stripMargin.toLisa
    val factFunction =
      factWithTailRec
        .pipe(Evaluator.eval(_, Preludes.preludeEnvironment))
        .pipe(_.asInstanceOf[Evaluator.EvalSuccess]).env.getValueOption("fact").get.asInstanceOf[Closure]
    assertResult(factFunction.freeVariables)(Set("*", "-"))
    val factOf5 = factFunction(5).pipe(Evaluator.eval(_, EmptyEnv)).asInstanceOf[EvalSuccess].expression
    assertResult(factOf5)(SInteger(120))
    val factOf10000 = factFunction(10000).pipe(Evaluator.eval(_, EmptyEnv)).asInstanceOf[EvalSuccess].expression
    assert(factOf10000.isInstanceOf[SInteger])
  }

  test("String interpolation should work well") {
    assert(
      lisa"""$$"A raw string"""".isInstanceOf[SString], "Empty template should be string only."
    )
    assertResult(
      lisa""" $$"1 + 1 = $${(+ 1 1)}" """
    )(Apply(/*symbol"string"*/Preludes.stringFunction, "1 + 1 = " :: Apply(symbol"+", 1.asLisa :: 1.asLisa :: Nil) :: "".asLisa :: Nil))
    assertThrows[Exception](lisa"""s "" """)
    assertResult(
      lisa""" s"$$a-b" """
    )(Apply(symbol"s", LisaList.fromExpression("", "-b") :: Apply(symbol"list", symbol"a" :: Nil) :: Nil))
    assertResult(lisa""":"atom$$1"""")(SAtom("atom$1"))
  }
}
