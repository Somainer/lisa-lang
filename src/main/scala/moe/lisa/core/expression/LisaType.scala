package moe.lisa.core.expression

import moe.lisa.core.expression.Tree._

trait LisaType extends Expression[LisaType] {
  override type SelfType[-T >: Untyped] <: LisaType
  override def isType: Boolean = true
}
