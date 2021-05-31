package moe.roselia.lisa.Compile.Phases

import moe.roselia.lisa.LispExp._

object IifeInline extends Phase with ExpressionTraverse {
  override def transform(expression: Expression): Expression = traverseExpressionBody(expression) {
    case Apply(LambdaExpression(returnValue, Nil, body), Nil) => body :+ returnValue
    case Apply(Symbol("group!" | "block"), body) => body
  }
}
