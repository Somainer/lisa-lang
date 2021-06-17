package moe.lisa.compile.transform

import com.somainer.nameof.NameOf._
import moe.lisa.core.Phases.Phase
import moe.lisa.core.context.Context
import moe.lisa.core.context.Context._
import moe.lisa.core.SourceFile.NoSourceFile
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Tree.Symbol
import moe.lisa.core.expression.UntypedTree
import moe.lisa.lang.{DynamicPredef, Symbol => LSymbol, Predef => LPredef}
import moe.lisa.parsing.ParseTimeTraverser
import moe.lisa.parsing.ParseTimeDepCollector

object ApplyStringTemplate extends Phase, ParseTimeTraverser, MiniTransform {
  override def name: String = nameOf(ApplyStringTemplate)
  override def run(using Context): Unit =
    val unit = ctx.compilationUnit
    unit.untypedTree = applyStringTemplate(unit.untypedTree)

  def applyStringTemplate(tree: Tree): Tree = traverse(tree) {
    case StringTemplate(Symbol(LSymbol("$")), part :: Nil, Nil) => part
    case st @ StringTemplate(templateName, parts, arguments) =>
      val predefNamespace = qualifiedNameOfType[LPredef]
      val rtListSymbol = LSymbol(s"$predefNamespace.${nameOf(LPredef.list(???))}")
      val rtStringSymbol = LSymbol(s"$predefNamespace.${nameOf(LPredef.`string`(???))}")
      val listSymbol = UntypedTree.Symbol(rtListSymbol)(using NoSourceFile)
      if templateName.name.fullName == "$" then
        cpy.LisaList(st)(cpy.Symbol(templateName)(rtStringSymbol) :: {
          parts.head :: arguments.zip(parts.tail).flatten(List(_, _))
        })
      else
        cpy.LisaList(st)(
          templateName ::
            cpy.LisaList(UntypedTree.Thicket(parts)(using NoSourceFile))(listSymbol :: parts) ::
            cpy.LisaList(UntypedTree.Thicket(arguments)(using NoSourceFile))(listSymbol :: arguments) :: Nil
        )
  }

  override def transform(tree: Tree): Tree =
    applyStringTemplate(tree)
}
