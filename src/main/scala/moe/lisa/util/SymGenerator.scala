package moe.lisa.util

import java.util.concurrent.atomic.AtomicInteger

class SymGenerator:
  private val counter = AtomicInteger()
  private[lisa] def nextCount(): Int = counter.getAndIncrement()
  def nextSymbol = Symbol(s"_LISA_IMAI__${nextCount()}")

object SymGenerator:
  lazy val defaultGenerator: SymGenerator = SymGenerator()

  export defaultGenerator.{nextCount, nextSymbol}
