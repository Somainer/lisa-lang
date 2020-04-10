package moe.roselia.lisa.Logical

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.LispExp.{Expression, LisaList, LisaMapRecord, NameOnlyType, NilObj, PrimitiveFunction, PrimitiveMacro, Quote, SAtom, SBool, SString, SideEffectFunction, Symbol, TypedLisaRecord, WrappedScalaObject}

case class LogicalEnvironment(var logicalContext: LogicalContext) extends Queries {
  implicit def context: LogicalContext = logicalContext

  def addFact(fact: Expression): Unit = {
    logicalContext = logicalContext.addedFact(replaceIfPossible(fact, {
      case s@Symbol("_") => s
      case Symbol(s) => SAtom(s)
      case Quote(sym@Symbol(_)) => sym
    }))
  }
  def addRule(name: String, rule: LogicalRule): Unit = {
    logicalContext = logicalContext.addedRule(name, rule)
  }

  def executeQuery(query: Expression, inEnvironment: Environment): Expression = {
    matchResultToLisaNative(runMatch(query, inEnvironment))
  }

  def factExists(query: Expression, inEnvironment: Environment): SBool = {
    SBool(
      runMatch(query, inEnvironment).nonEmpty
    )
  }

  def runMatch(query: Expression, inEnvironment: Environment): OutputType = {
    Matcher.runMatcher(
      compileExpressionToMatcher(query, context, inEnvironment)
    )
  }

  lazy val implementationsEnvironment: Environment = EmptyEnv.withValues(Seq(
    "fact" -> PrimitiveMacro {
      case (fact :: Nil, e) =>
        addFact(fact.toRawList)
        NilObj -> e
      case (xs, _) => throw new IllegalArgumentException(s"1 argument expected but ${xs.length} found.")
    },
    "add-fact" -> PrimitiveFunction.withArityChecked(1) {
      case ex :: Nil =>
        addFact(ex)
        NilObj
    },
    "get-facts" -> PrimitiveFunction.withArityChecked(0) { case _ =>
      context.facts
    },
    "define-rule" -> PrimitiveMacro { case (xs, e) =>
      xs.map(_.toRawList) match {
        case LisaList(Symbol(name) :: args) :: body :: Nil =>
          addRule(name, LogicalRule.createNew(args, body.toRawList))
          NilObj -> e
        case LisaList(Symbol(name) :: args) :: Nil => // rule with empty body will always be true.
          addRule(name, LogicalRule.createNew(args, SBool(true)))
          NilObj -> e
      }
    },
    "query" -> PrimitiveMacro {
      case (body :: Nil, e) =>
        executeQuery(body.toRawList, e) -> e
    },
    "lazy-query"-> PrimitiveMacro {
      case (body :: Nil, e) =>
        WrappedScalaObject(
          runMatch(body.toRawList, e).map(m => LisaMapRecord(m.flattenToMap))
        ) -> e
    },
    "run-query" -> SideEffectFunction { case (body :: Nil, e) =>
      executeQuery(body, e) -> e
    },
    "run-lazy-query"-> SideEffectFunction {
      case (body :: Nil, e) =>
        WrappedScalaObject(
          runMatch(body, e).map(m => LisaMapRecord(m.flattenToMap))
        ) -> e
    },
    "is-true?" -> PrimitiveMacro {
      case (body :: Nil, e) =>
        factExists(body.toRawList, e) -> e
    },
    "test-is-true?" -> SideEffectFunction {
      case (body :: Nil, e) =>
        factExists(body, e) -> e
    },
    "current-context" -> PrimitiveFunction.withArityChecked(0) { case _ =>
      TypedLisaRecord(Map(
        "facts" -> context.facts,
        "rules" -> LisaMapRecord(context.rules.view.mapValues(WrappedScalaObject(_)).toMap, "LogicalRules")
      ), NameOnlyType("LogicalContext"))
    },
    "current-context/raw" -> PrimitiveFunction.withArityChecked(0) { case _ =>
      WrappedScalaObject(context)
    },
    "pop-context!" -> SideEffectFunction { case (_, e) =>
      NilObj -> e.collectBy(_ ne implementationsEnvironment)
    }
  ))
}
