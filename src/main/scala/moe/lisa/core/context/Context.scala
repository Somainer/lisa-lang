package moe.lisa.core.context

import dotty.tools.dotc.reporting.Reporter

import moe.lisa.core.context.Store
import moe.lisa.core.CompilationUnit
import moe.lisa.core.SourceFile._

trait Context(base: ContextBase) {
  import Context._
  given Context = this

  var store: Store = Context.store

  def compilationUnit: CompilationUnit = store(complicationUnitLoc)

  private var _source: SourceFile = _
  final def source: SourceFile = _source
}

object Context:
  private val ((complicationUnitLoc, _), store) = 
    Store.empty.newLocations[(CompilationUnit, String)]

  def ctx(using c: Context): Context = c
