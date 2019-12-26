package moe.roselia.lisa.Util

import java.util.concurrent.atomic.AtomicInteger

import moe.roselia.lisa.LispExp

class SymGenerator {
  private val innerCounter = new AtomicInteger

  private[lisa] def nextCount: Int = innerCounter.getAndIncrement()

  def nextSym = LispExp.Symbol(s"_LISA_IMAI__$nextCount")
}

object SymGenerator {
  lazy val defaultGenerator = new SymGenerator

  def nextSym: LispExp.Symbol = defaultGenerator.nextSym
}
