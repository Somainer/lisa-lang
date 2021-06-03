package moe.lisa.compiletime

import moe.lisa.core.expression.LisaType
import moe.lisa.core.expression.Properties._
import moe.lisa.core.expression.Tree._
import moe.lisa.lang.{IRecord, LisaRecord}

object TreeOps {
//  def `get-doc`(tree: Tree[?]): String = tree.getAttachment(DocString).getOrElse("")
  def `get-meta`(tree: Tree[?]): LisaRecord =
    tree.toMetaRecord
  
  def `set-meta`(tree: Tree[?], record: LisaRecord): Unit =
    tree.restoreMeta(record)
    
  def `type-of`(tree: Tree[?]): LisaType = tree.tpe
}
