package moe.lisa.repl

import dotty.tools.repl.ReplCompiler
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.repl.results.Result
import dotty.tools.repl.Parsed
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.report
import dotty.tools.dotc.config.Settings.Setting._

import moe.lisa.compile.transform.GenAst
import moe.lisa.compile.GenScalaCode
import moe.lisa.core.exceptions.LisaSyntaxError
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.SourceFile.NoSourceFile

class LisaReplCompiler extends ReplCompiler:
  def compileLisa(trees: List[Tree])(using state: ReplState): Result[(CompilationUnit, ReplState)] =
    given Context = state.state.context
    val imports = 0.until(state.state.objectIndex).map { idx =>
      val name = ReplCommand.sourceFileName(idx + 1)
      untpd.Import(untpd.Ident(termName(name)), untpd.ImportSelector(untpd.Ident(nme.WILDCARD)) :: Nil)
    }.toList
    val scalaTrees = imports ::: transformLisaAst(trees)
    if state.state.context.settings.YprintDebug.value then
      println(scalaTrees.map(_.show).mkString("\n"))
    compile(Parsed(trees.headOption.map(_.source).getOrElse(NoSourceFile), scalaTrees))(state.state)
      .map((cu, st) => (cu, state.copy(st)))

  def transformLisaAst(trees: List[Tree])(using state: ReplState): List[untpd.Tree] = {
    given Context = state.state.context
    val gen = new GenScalaCode()
    def transformOne(tree: Tree) =
      reported(whenErr = untpd.EmptyTree) {
        val ast = GenAst.transformBodyExpression(tree)
        gen.transform(ast)
      }
    val scalaTrees = trees.map(transformOne)
    gen.defaultImports ::: scalaTrees
  }

  def reported[T](whenErr: => T)(op: Context ?=> T)(using state: ReplState): T =
    given Context = state.state.context
    try op
    catch
      case err: LisaSyntaxError[?] =>
        report.error(err.message, err.position, true)
        whenErr

  def astGenReported(tree: Tree)(using ReplState): Tree = {
    reported(GenAst.nilObjWhere(tree.source).withSpan(tree.span)) {
      GenAst.transform(tree)
    }
  }
