package moe.lisa.core.expression

import Tree._

trait Expression[-T >: Untyped] extends Tree[T] {
  override type SelfType[T >: Untyped] <: Expression[Untyped]

  override def isExpression: Boolean = true
}

trait ToExpression[-T] {
  def toExpression(v: T): Expression[Any]
}
