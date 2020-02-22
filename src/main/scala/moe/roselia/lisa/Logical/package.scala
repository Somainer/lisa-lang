package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, NameSpacedEnv, TransparentLayer}
import moe.roselia.lisa.LispExp.{LisaList, LisaMapRecord, LisaRecordWithMap, NilObj, PrimitiveFunction, SideEffectFunction, WrappedScalaObject}

package object Logical {
  import Util.Extractors.RichOption
  lazy val LogicalModuleEnvironment: NameSpacedEnv = NameSpacedEnv("logical", EmptyEnv.withValues(Seq(
    "push-context" -> SideEffectFunction {
      case (WrappedScalaObject(ctx: LogicalContext) :: Nil, e) =>
        val environment = LogicalEnvironment(ctx)
        NilObj -> TransparentLayer(environment.implementationsEnvironment, e)
      case ((lr: LisaRecordWithMap[_]) :: Nil, e) =>
        val facts = lr.record.get("facts").collect {
          case ll: LisaList[_] => ll
        }.getOrThrow(new NoSuchElementException("facts"))
        val rules = lr.record.get("rules").collect {
          case lr: LisaRecordWithMap[_] =>
            lr.record.view.mapValues {
              case WrappedScalaObject(rule: LogicalRule) => rule
            }.toMap
        }.getOrThrow(new NoSuchElementException("rules"))
        val context = LogicalContext(facts, rules)
        val environment = LogicalEnvironment(context)
        NilObj -> TransparentLayer(environment.implementationsEnvironment, e)
    },
    "new-context" -> SideEffectFunction {
      case (_, e) =>
        NilObj -> TransparentLayer(
          LogicalEnvironment(LogicalContext.empty).implementationsEnvironment,
          e
        )
    },
    "unify" -> PrimitiveFunction.withArityChecked(2) {
      case lhs :: rhs :: Nil =>
        Queries.unifyMatch(lhs, rhs, EmptyEnv.newMutableFrame)
          .map(_.flattenToMap)
          .map(LisaMapRecord(_))
          .getOrElse(NilObj)
    }
  )), "/")
}
