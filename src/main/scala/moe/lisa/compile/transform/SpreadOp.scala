package moe.lisa.compile.transform

import com.somainer.nameof.NameOf._
import moe.lisa.core.Phases.Phase
import moe.lisa.core.context.Context
import moe.lisa.core.context.Context._
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Tree.Symbol
import moe.lisa.core.expression.UntypedTree
import moe.lisa.lang.{Symbol => LSymbol}
import moe.lisa.parsing.ParseTimeTraverser

object SpreadOp extends Phase, ParseTimeTraverser, MiniTransform {
  override def name: String = nameOf(SpreadOp)

  override def run(using Context): Unit =
    val unit = ctx.compilationUnit
    unit.untypedTree = makeSpreads(unit.untypedTree)

  def makeSpreads(tree: Tree): Tree = traverse(tree) {
    case s @ Symbol(LSymbol(s"...$name")) =>
      val modifiedName = if name.isEmpty then "_" else name
      cpy.Spread(s)(cpy.Symbol(s)(LSymbol(modifiedName)))
    case s @ LisaList(Symbol(LSymbol("...")) :: ex :: Nil) =>
      cpy.Spread(s)(ex)
  }

  override def transform(tree: Tree): Tree =
    makeSpreads(tree)
}