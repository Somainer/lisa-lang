package moe.lisa.compile

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.Reporter

class LisaDriver extends Driver {
  private def lisaCompiler = new LisaCompiler
  def compileLisa(args: Array[String], rootCtx: Context): Reporter =
    setup(args, rootCtx) match {
      case Some((files, context)) =>
        doCompile(lisaCompiler, files)(using context)
      case _ =>
        rootCtx.reporter
    }
    
  override def process(args: Array[String], rootCtx: Context): Reporter = compileLisa(args, rootCtx)
}
