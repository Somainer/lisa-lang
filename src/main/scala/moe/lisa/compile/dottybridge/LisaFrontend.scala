package moe.lisa.compile.dottybridge

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.dotc.report
import dotty.tools.dotc.util.{SourcePosition, Spans}
import moe.lisa.compile.GenScalaCode
import moe.lisa.compile.transform.GenAst
import moe.lisa.core.exceptions.LisaSyntaxError
import moe.lisa.parsing.LisaParser
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.util.ThrowHelper

class LisaFrontend extends FrontEnd {
  extension(unit: CompilationUnit)
    def isLisa: Boolean = unit.source.file.name.endsWith(".lisa")
  override def parse(using Context): Unit =
    val unit: CompilationUnit = ctx.compilationUnit
    if unit.isLisa then
      compileLisa
    else super.parse

  def compileLisa(using Context): Unit =
    val unit: CompilationUnit = ctx.compilationUnit
    for tree <- parseLisa do
      try
        val ast = GenAst.transform(tree)
        val gen = new GenScalaCode
        unit.untpdTree = gen(ast)
      catch
        case ls: LisaSyntaxError[?] =>
          report.error(ls.message, ls.position, true)

  def parseLisa(using Context): Option[PackageDef] =
    val unit: CompilationUnit = ctx.compilationUnit
    assert(unit.isLisa)
    val parser = LisaParser.ofSource(using unit.source)
    parser.parsePakcage(unit.source) match
      case parser.Success(res, _) =>
        Some(res)
      case parser.NoSuccess(msg, loc) =>
        val pos = loc.pos
        val srcPos = SourcePosition(unit.source, Spans.Span(loc.offset))
        report.error(msg, srcPos, sticky = true)
        None
      case _ => ThrowHelper.unreachable()
}
