package moe.roselia.lisa.Library

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.{Environments, Evaluator}
import moe.roselia.lisa.LispExp.{Apply, LisaList, NilObj, PrimitiveFunction, Procedure, SideEffectFunction, WrappedScalaObject}

object ContextualFunction {
  def contextualFunction(procedure: Procedure): SideEffectFunction = {
    SideEffectFunction { (args, env) =>
      Evaluator.eval(Apply(procedure, List(LisaList(args), WrappedScalaObject(env))), EmptyEnv) match {
        case f: Evaluator.EvalFailure => throw new RuntimeException(f.toString)
        case Evaluator.EvalSuccess(ex, _) =>
          ex match {
            case LisaList(result :: WrappedScalaObject(e: Environment) :: Nil) =>
              (result, e)
            case result => (result, env)
          }
      }
    }
  }

  lazy val ContextualFunctionEnv: Environments.Environment = EmptyEnv.withValues(Seq(
    "side-effect" -> PrimitiveFunction.withArityChecked(1) {
      case (procedure: Procedure) :: Nil => contextualFunction(procedure)
      case e :: Nil =>
        throw new IllegalArgumentException(s"Expected a procedure, but $e: ${e.tpe.name} found.")
    }
  ))
}
