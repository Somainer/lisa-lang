package moe.lisa.parsing

import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier as cpy}
import moe.lisa.core.expression.UntypedTree._

trait ParseTimeTraverser {
  def traverseTree(tree: Tree, visitor: Tree ?=> PartialFunction[Tree, Tree]): Tree =
    given Tree = tree
    tree match
      case tr if visitor.isDefinedAt(tr) =>
        traverseTree(visitor(tr), visitor)
      case Quote(e) =>
        cpy.Quote(tree)(traverseTree(e, visitor))
      case UnQuote(e) =>
        cpy.UnQuote(tree)(traverseTree(e, visitor))
      case LisaList(values) =>
        cpy.LisaList(tree)(values.mapConserve(traverseTree(_, visitor)))
      case RecordLiteral(values) =>
        cpy.RecordLiteral(tree)(values.mapConserve(traverseTree(_, visitor)))
      case st @ StringTemplate(templateName, parts, arguments) =>
        StringTemplate(templateName, parts, arguments.mapConserve(traverseTree(_, visitor)))(using st.source)
      case pd @ PackageDef(pid, stats) =>
        cpy.PackageDef(pd)(pid, stats.mapConserve(traverseTree(_, visitor)))
      case sp @ Spread(spread) =>
        cpy.Spread(sp)(traverseTree(spread, visitor))
      case df @ Define(sym, value) =>
        cpy.Define(df)(traverseTree(sym, visitor), traverseTree(value, visitor))
      case _ => tree
      
  def traverse(tree: Tree)(visitor: Tree ?=> PartialFunction[Tree, Tree]): Tree = traverseTree(tree, visitor)

  inline final def currrentTree(using tree: Tree): Tree = tree
}

object ParseTimeTraverser extends ParseTimeTraverser
