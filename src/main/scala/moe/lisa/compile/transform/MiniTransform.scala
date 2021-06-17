package moe.lisa.compile.transform

import moe.lisa.core.expression.UntypedTree
import moe.lisa.core.expression.UntypedTree._

trait MiniTransform {
  def transform(tree: Tree): Tree
}

object MiniTransform:
  case class ChainTransformer(transforms: List[MiniTransform]) extends MiniTransform:
    override def transform(tree: Tree): Tree =
      transforms.foldLeft(tree)((tr, trans) => trans.transform(tr))
      
    final def chain(that: MiniTransform): ChainTransformer = that match
      case ChainTransformer(trans) => ChainTransformer(transforms ::: trans)
      case _ => ChainTransformer(transforms :+ that)
      
    final inline def >>(that: MiniTransform): ChainTransformer = this chain that
    
    final infix def withOut(that: MiniTransform): ChainTransformer = copy(transforms.filterNot(_ eq that))
  
  val NoTransform = ChainTransformer(Nil)
  
  extension (mt: MiniTransform)
    def chain(that: MiniTransform): ChainTransformer =
      NoTransform.chain(mt).chain(that)

    inline def >>(that: MiniTransform): ChainTransformer = mt chain that
