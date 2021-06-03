package moe.lisa.parsing

import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier as cpy}
import moe.lisa.core.expression.UntypedTree._

trait ParseTimeTraverser {
  def traverseTree(tree: Tree, visitor: PartialFunction[Tree, Tree]): Tree =
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
      case _ => tree
      
  def traverse(tree: Tree)(visitor: PartialFunction[Tree, Tree]): Tree = traverseTree(tree, visitor)
}

object ParseTimeTraverser extends ParseTimeTraverser
