package moe.roselia.lisa.Logical

import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.Expression

case class LogicalRule(variants: List[(List[Expression], Expression)]) {
  def addNewVariant(params: List[Expression], body: Expression): LogicalRule =
    copy(variants :+ (params, body))
  def mergeVariants(that: LogicalRule): LogicalRule = copy(variants ::: that.variants)

  def findMatch(params: List[Expression]): Option[(Map[String, Expression], Expression)] = {
    variants.view.flatMap { case (pattern, body) =>
      Evaluator.matchArgument(pattern, params).map(_ -> body)
    }.headOption
  }
}
object LogicalRule {
  def empty: LogicalRule = LogicalRule(Nil)
  def createNew(params: List[Expression], body: Expression): LogicalRule =
    empty.addNewVariant(params, body)
}
