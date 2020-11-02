package moe.roselia.lisa.Library

import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{LisaList, NilObj, Procedure, SideEffectFunction, WrappedScalaObject}

object ContextualFunction {
  def contextualFunction(procedure: Procedure): SideEffectFunction = {
    SideEffectFunction { (args, env) =>
      Evaluator.apply(procedure, List(LisaList(args), WrappedScalaObject(env))) match {
        case x => (NilObj, env)
      }
    }
  }
}
