import moe.roselia.lisa
import lisa.LispExp._
import moe.roselia.lisa.Environments.{CombineEnv, Environment}
import moe.roselia.lisa.{Environments, Evaluator, Preludes}
import moe.roselia.lisa.Evaluator.{EvalFailure, EvalSuccess, expandMacro}
import moe.roselia.lisa.Exceptions.LisaRuntimeException

trait ExpressionHelper extends Implicits {
  def define(sym: Expression, body: Expression) = Define(sym, body)

  def lambdaExpression(args: Seq[Expression], body: Expression*) =
    LambdaExpression(body.last, args.toList, body.init.toList)

  def defun(sym: Symbol, args: Seq[Expression], body: Expression*): Define =
    define(sym, lambdaExpression(args, body: _*))

  implicit class StringHelper(sc: StringContext) {
    def symbol(arg: Any*) = Symbol(sc.s(arg: _*))
    def lisa(arg: Any*): Expression = sc.s(arg: _*).toLisa
  }

  implicit class ExpressionOps(ex: Expression) {
    def apply(args: Expression*): Expression = Apply(ex, args.toList)
    def quoted = Quote(ex)
    def evalOn(env: Environment): Expression =
      Evaluator.eval(ex, env) match {
        case EvalSuccess(expression, _) => expression
        case f@EvalFailure(_, _, _) =>
          throw LisaRuntimeException(ex, new RuntimeException(f.toString))
      }
    def evalOnPrelude = evalOn(Preludes.preludeEnvironment)
    def evalOnEmptyEnv = evalOn(Environments.EmptyEnv)
  }

  implicit class ExpressionListOps(exs: List[Expression]) {
    def evalOn(env: Environment): List[Expression] = {
      @scala.annotation.tailrec
      def traverse(exp: List[Expression], env: Environment, acc: List[Expression] = Nil): List[Expression] = exp match {
        case Nil => acc.reverse
        case x :: xs => Evaluator.eval(x, env) match {
          case EvalSuccess(ee, ev) => traverse(xs, ev, ee :: acc)
          case f => throw LisaRuntimeException(x, new RuntimeException(f.toString))
        }
      }
      traverse(exs, env)
    }
  }

  implicit class StringOps(string: String) {
    def asSymbol = Symbol(string)
    def asAtom = SAtom(string)
    def toLisa: Expression = {
      val program = string
      import moe.roselia.lisa.SExpressionParser._
      import moe.roselia.lisa.LispExp
      val lisp = parseAll(sExpression, program).get
      moe.roselia.lisa.Evaluator.compile(lisp).ensuring(!_.isInstanceOf[LispExp.Failure])
    }
    def toListOfLisa: List[Expression] = {
      val program = string
      import moe.roselia.lisa.SExpressionParser._
      import moe.roselia.lisa.LispExp
      val lisps = parseAll(rep(sExpression), program).get
      lisps.map(Evaluator.compile).ensuring(_.forall(!_.isInstanceOf[LispExp.Failure]))
    }
  }

  implicit class AnyToLisa[T](value: T)(implicit lift: T => Expression) {
    def asLisa: Expression = lift(value)
  }

  implicit class EnvHelpers(env: Environment) {
    def combinedWithPrelude: Environment = CombineEnv(Seq(
      Preludes.preludeEnvironment, env
    ))
  }

}

object ExpressionHelper extends ExpressionHelper
