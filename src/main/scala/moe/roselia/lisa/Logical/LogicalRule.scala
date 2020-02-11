package moe.roselia.lisa.Logical

import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.Expression

case class LogicalRule(variants: List[(List[Expression], Expression)]) {
  def addNewVariant(params: List[Expression], body: Expression): LogicalRule =
    copy(variants :+ (params, body))
  def mergeVariants(that: LogicalRule): LogicalRule = copy(variants ::: that.variants)

  def findMatch(params: List[Expression]): Seq[((Map[String, Expression], Map[String, Expression]), Expression)] = {
    /**
     * Seq[(matchedConstraints, introducedVariables), ruleBody]
     */
    variants.view.flatMap { case (pattern, body) =>
      LogicalRule.matchLogicalRuleArgument(pattern, params).map(_ -> body)
    }.toSeq
  }
}
object LogicalRule {
  def empty: LogicalRule = LogicalRule(Nil)
  def createNew(params: List[Expression], body: Expression): LogicalRule =
    empty.addNewVariant(params, body)

  private type MatchConstraint = collection.mutable.Map[String, Expression]
  private type FrozenMatchConstraint = Map[String, Expression]
  def matchLogicalRuleArgument(pattern: List[Expression],
                               data: List[Expression]): Option[(FrozenMatchConstraint, FrozenMatchConstraint)] = {
    /** (define-rule (f :a b) ...)
     *  (query (f x y))
     *  [[pattern]]: (:a b)
     *  [[data]]: (x y)
     *  [[constraints]]: => b -> y
     *  [[introduced]]: => x -> :a
     */
    val introduced: MatchConstraint = collection.mutable.Map.empty
    val constraints: MatchConstraint = collection.mutable.Map.empty
    import moe.roselia.lisa.LispExp._
    def succeed = Some(constraints -> introduced)

    def extend(name: String, exp: Expression, constraint: MatchConstraint): Option[(MatchConstraint, MatchConstraint)] = {
      if(name == "_") succeed
      else constraint.get(name) match {
        case Some(`exp`) => succeed
        case Some(_) => None
        case _ =>
          constraint.update(name, exp)
          succeed
      }
    }

    def matchOne(pattern: Expression, data: Expression): Option[(MatchConstraint, MatchConstraint)] = {
      if(pattern == data) {
        pattern match {
          case Symbol(sym) => constraints.update(sym, data)
          case _ =>
        }
        succeed
      }
      else pattern match {
        case Symbol(name) => extend(name, data, constraints)
        case _ => data match {
          case Symbol(name) => extend(name, pattern, introduced)
          case LisaList(ll2) => data match {
            case LisaList(ll1) => matchMany(ll1, ll2)
            case _ => None
          }
          case _ => None
        }
      }
    }
    def matchMany(pattern: List[Expression], data: List[Expression]): Option[(MatchConstraint, MatchConstraint)] = {
      pattern match {
        case Nil => if (data.isEmpty) succeed else None

        case LisaList(Symbol("...") :: (sym@Symbol(_)) :: Nil) :: xs =>
          if (xs.isEmpty) {
            matchOne(sym, LisaList(data))
          }
          else None
        case otherwise :: xs => data match {
          case y :: ys => for {
            _ <- matchOne(otherwise, y)
            tailMatch <- matchMany(xs, ys)
          } yield tailMatch
          case _ => None
        }
      }
    }

    matchMany(pattern, data).map { case (a, b) =>
      (a.toMap, b.toMap)
    }
  }
}
