package moe.lisa.core.expression

import moe.lisa.core.SourceFile._

object TypedTree extends TreeInstance[LisaType] {
  class EmptyTree extends Thicket(Nil)(using NoSourceFile)
  object EmptyTree extends EmptyTree
}
