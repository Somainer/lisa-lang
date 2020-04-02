package moe.roselia.lisa.Logical

import moe.roselia.lisa.LispExp.{Closure, Expression, LisaList, LisaMapRecord, Procedure, SAtom}

case class LogicalContext(facts: LisaList[Expression], rules: Map[String, LogicalRule]) {
  def addedRule(name: String, rule: LogicalRule): LogicalContext = {
    copy(rules = rules.updated(name, rules.getOrElse(name, LogicalRule.empty).mergeVariants(rule)))
  }
  def addedFact(fact: Expression): LogicalContext =
    copy(facts = LisaList(facts.list :+ fact))

  def hasRule(name: String): Boolean = rules contains name
  def getRule(name: String): Option[LogicalRule] = rules.get(name)

  def findRule(name: String, params: List[Expression]): Seq[((Map[String, Expression], Map[String, Expression]), Expression)] = {
    rules.get(name).map(_.findMatch(params)).getOrElse(Nil)
  }

  def factNames: Set[String] = facts.collect {
    case LisaList(SAtom(value) :: _) => value
  }.toSet
  def ruleNames: Set[String] = rules.keySet

  def definitionNames: List[String] = factNames.concat(ruleNames).toList
}
object LogicalContext {
  def empty: LogicalContext = LogicalContext(LisaList.empty, Map.empty)
}