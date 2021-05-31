package moe.roselia.lisa.Compile.Phases

import moe.roselia.lisa.LispExp._

trait ExpressionTraverse {
  def wrapManyToOne(fns: Seq[Expression]): Expression = {
    if (fns.isEmpty) NilObj
    else if (fns.length == 1) fns.head
    else Apply(LambdaExpression(fns.last, Nil, fns.init.toList), Nil)
  }

  def traverseExpressionBody(expression: Expression)(visitor: PartialFunction[Expression, Seq[Expression]]): Expression = {
    wrapManyToOne(traverseExpressionBodyImpl(expression, visitor))
  }

  private def traverseExpressionBodyImpl(expression: Expression, visitor: PartialFunction[Expression, Seq[Expression]]): Seq[Expression] = {
    def traverse(newExpr: Expression): Seq[Expression] = {
      if (expression != newExpr) traverseExpressionBodyImpl(newExpr, visitor)
      else Seq(newExpr)
    }

    def traverseToOneExpression(expression: Expression): Expression =
      wrapManyToOne(traverse(expression))

    expression match {
      case exp if visitor.isDefinedAt(exp) =>
        val visited = visitor(exp)
        if (visited != Seq(exp)) visitor(exp).flatMap(traverse)
        else visited
      case LambdaExpression(body, boundVariable, nestedExpressions) =>
        val fullBody = nestedExpressions.flatMap(traverse) ++ traverse(body)
        traverse(LambdaExpression(fullBody.last, boundVariable, fullBody.init))
      case SIfElse(predicate, consequence, alternative) =>
        traverse(SIfElse(traverseToOneExpression(predicate), traverseToOneExpression(consequence), traverseToOneExpression(alternative)))
      case Apply(head, args) =>
        traverse(Apply(traverseToOneExpression(head), args.flatMap(traverse)))
      case SCond(conditions) =>
        traverse(SCond(conditions.map {
          case (condition, consequence) =>
            (traverseToOneExpression(condition), traverseToOneExpression(consequence))
        }))
      case Define(symbol, value) =>
        traverse(Define(symbol, traverseToOneExpression(value)))
      case Quote(exp, true) =>
        traverse(Quote(traverseToOneExpression(exp), isQuasiQuote = true))
      case quote: Quote => traverse(quote)
      case UnQuote(exp, splicing) =>
        traverse(UnQuote(traverseToOneExpression(exp), splicing))
      case exp => Seq(exp)
    }
  }
}
