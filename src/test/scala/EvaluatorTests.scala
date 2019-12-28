import moe.roselia.lisa.Environments.EmptyEnv
import moe.roselia.lisa.Evaluator.EvalSuccess
import org.scalatest.funsuite._
import org.scalatest.Assertions._
import moe.roselia.lisa._

class EvaluatorTests extends AsyncFunSuite {

  import LispExp.Implicits._
  import LispExp._
  import util.chaining._

  test("Factorial function should work well") {
    val factZero = Evaluator.eval(Define(Symbol("fact"), LambdaExpression(1, 0 :: Nil)), Preludes.preludeEnvironment)
    assert(factZero.isSuccess)
    val Evaluator.EvalSuccess(`NilObj`, env) = factZero
    assert(env.getValueOption("fact").get.isInstanceOf[Closure])
    val finallyFact = Evaluator.eval(Define(Symbol("fact"),
      LambdaExpression(
        Apply(Symbol("*"), Symbol("n") :: Apply(Symbol("fact"), List(Apply(Symbol("-"), List(Symbol("n"), 1)))) :: Nil),
        List(Symbol("n")))), env)
    val EvalSuccess(`NilObj`, factEnv) = finallyFact
    val fact = factEnv.getValueOption("fact").get
    assert(fact.isInstanceOf[PolymorphicExpression])
    val EvalSuccess(num, _) = Evaluator.eval(Apply(fact, 5 :: Nil), EmptyEnv)
    assertResult(num)(SInteger(120))
    assertThrows[StackOverflowError](Evaluator.eval(Apply(fact, 260 :: Nil), EmptyEnv))
  }

  test("Tall call optimization should work well") {
    import SExpressionParser._
    val parseResult = parseAll(sExpression,
      """
        |(define (fact n)
        |   (define (f 0 x) x)
        |   (define (f n x) (f (- n 1) (* x n)))
        |   (f n 1))
        |""".stripMargin)
    assert(parseResult.successful)
    val factFunction =
      Evaluator.compile(parseResult.get)
        .pipe(Evaluator.eval(_, Preludes.preludeEnvironment))
        .pipe(_.asInstanceOf[Evaluator.EvalSuccess]).env.getValueOption("fact").get.asInstanceOf[Closure]
    assertResult(factFunction.freeVariables)(Set("*", "-"))
    val factOf5 = Apply(factFunction, 5 :: Nil).pipe(Evaluator.eval(_, EmptyEnv)).asInstanceOf[EvalSuccess].expression
    assertResult(factOf5)(SInteger(120))
    val factOf1000 = Apply(factFunction, 1000 :: Nil).pipe(Evaluator.eval(_, EmptyEnv)).asInstanceOf[EvalSuccess].expression
    assert(factOf1000.isInstanceOf[SInteger])
  }
}
