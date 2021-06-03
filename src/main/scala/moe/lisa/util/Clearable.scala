package moe.lisa.util

import scala.collection.{mutable => mutColl}

trait Clearable[-T]:
  def clear(coll: T): Unit

object Clearable:
  given Clearable[mutColl.Clearable] = _.clear()
  
  given Clearable[AutoClearable] = _.clear()
  
trait AutoClearable:
  private val clearCallbacks: mutColl.ArrayBuffer[() => Unit] = mutColl.ArrayBuffer.empty
  def needClear[T](coll: T)(using c: Clearable[T]): T =
    clearCallbacks += { () => c.clear(coll) }
    coll
    
  def clear(): Unit = clearCallbacks.foreach(_())
