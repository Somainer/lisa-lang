package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, NameSpacedEnv, TransparentLayer}
import moe.roselia.lisa.LispExp.{NilObj, SideEffectFunction, WrappedScalaObject}

package object Logical {
  lazy val LogicalModuleEnvironment: NameSpacedEnv = NameSpacedEnv("logical", EmptyEnv.withValues(Seq(
    "push-context" -> SideEffectFunction {
      case (WrappedScalaObject(ctx: LogicalContext) :: Nil, e) =>
        val environment = LogicalEnvironment(ctx)
        NilObj -> TransparentLayer(environment.implementationsEnvironment, e)
    },
    "new-context" -> SideEffectFunction {
      case (_, e) =>
        NilObj -> TransparentLayer(
          LogicalEnvironment(LogicalContext.empty).implementationsEnvironment,
          e
        )
    }
  )), "/")
}
