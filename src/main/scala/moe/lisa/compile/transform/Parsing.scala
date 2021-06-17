package moe.lisa.compile.transform

import com.somainer.nameof.NameOf._
import moe.lisa.core.context.Context
import Context._
import moe.lisa.core.CompilationUnit
import moe.lisa.core.Phases.Phase
import moe.lisa.parsing.LisaParser
import moe.lisa.util.ThrowHelper

object Parsing extends Phase {
  override def name: String = nameOf(Parsing)
  override def run(using Context): Unit =
    val parser: LisaParser = LisaParser.summon
    import parser._
    val unit: CompilationUnit = ctx.compilationUnit
    parseAll(sExpression, new String(unit.source.content())) match {
      case Success(tree, _) => unit.untypedTree = tree
      case parser.NoSuccess(reason, next) =>
        throw new RuntimeException(reason)
      case _ => ThrowHelper.unreachable() // This case never match.
    }
}
