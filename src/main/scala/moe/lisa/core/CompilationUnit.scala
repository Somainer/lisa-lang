package moe.lisa.core

import dotty.tools.dotc.util.SourceFile

import moe.lisa.util.SymGenerator
import moe.lisa.core.expression.UntypedTree
import moe.lisa.core.expression.TypedTree

class CompilationUnit (val source: SourceFile) {
  val symbolGenerator = SymGenerator()
  var untypedTree: UntypedTree.Tree = UntypedTree.EmptyTree
  var typedTree: TypedTree.Tree = TypedTree.EmptyTree
}
