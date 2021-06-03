package moe.lisa.core

import moe.lisa.core.context.Context

object Phases {
  trait Phase {
    def name: String
    def isRunnable(using Context): Boolean = true

    def run(using Context): Unit
    def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
      units.map { unit =>
        // TODO: Implement it.
        val unitContext: Context = summon
        run(using unitContext)
        unitContext.compilationUnit
      }
    def description: String = name
  }
}
