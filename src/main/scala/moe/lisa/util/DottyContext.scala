package moe.lisa.util

import dotty.tools.dotc.core.Contexts._

object DottyContext {
  val contextBase = new ContextBase
  contextBase.settings.YplainPrinter.value
  given Context = contextBase.initialCtx
}

trait DottyContext:
  export DottyContext.given
