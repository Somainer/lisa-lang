package moe.lisa.compile

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Phases
import moe.lisa.compile.dottybridge.LisaFrontend

class LisaCompiler extends Compiler {
  override def frontendPhases: List[List[Phases.Phase]] =
    val phases = super.frontendPhases
    List(new LisaFrontend) :: phases.tail
}