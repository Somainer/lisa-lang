package moe.roselia.lisa.Compile.Phases
import moe.roselia.lisa.LispExp.{Expression, IdenticalLisaExpression, LisaValue, NoExternalDependency}
import moe.roselia.lisa.{Environments, Evaluator, LispExp, Preludes}
import LispExp._
import moe.roselia.lisa.Environments.EmptyEnv
import moe.roselia.lisa.Evaluator.EvalSuccess

import java.util

object ConstantValueExpansion extends Phase with ExpressionTraverse {
  private val cache: util.IdentityHashMap[Expression, Boolean] = new util.IdentityHashMap()
  private def isPure(ex: Expression, inEnv: Environments.Environment): Boolean = {
    if (cache.containsKey(ex)) cache.get(ex)
    else {
      val purity = ex match {
        case Symbol(sym) if inEnv.has(sym) => isPure(inEnv.get(sym), inEnv)
        case Symbol(symbol) => !inEnv.directHas(symbol) && Preludes.purePreludeEnvironment.has(symbol)
        case fn: PrimitiveFunction => Preludes.pureFunctions contains fn
        case _: PrimitiveMacro => false
        case closure: Closure =>
          closure.freeVariables.map(closure.capturedEnv.get).filter(_ != closure).forall(isPure(_, closure.capturedEnv))
        case polymorphicExpression: PolymorphicExpression =>
          // Can not count it self or it will cause infinite loop.
          cache.put(polymorphicExpression, true)
          polymorphicExpression.variants.map(_._1)
            .forall(isPure(_, EmptyEnv))
        case _: LisaValue | _: IdenticalLisaExpression | _: NoExternalDependency => true
        case _ => false
      }
      cache.put(ex, purity)
      purity
    }
  }

  override def transform(expression: LispExp.Expression, environment: Environments.Environment): LispExp.Expression = {
    cache.clear()
    traverseExpressionBody(expression) {
      case ap@Apply(head, args) if isPure(head, environment) && args.forall(isPure(_, environment)) =>
        Evaluator.eval(ap, environment) match {
          case EvalSuccess(result, _) => Seq(result)
          case _ => Seq(ap)
        }
    }
  }
}
