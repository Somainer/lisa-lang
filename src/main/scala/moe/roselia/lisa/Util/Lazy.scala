package moe.roselia.lisa.Util

class Lazy[T] private (thunk: => T) {
  private var option: Option[T] = None

  def get: T = option match {
    case Some(value) => value
    case None =>
      val value = thunk
      option = Some(value)
      value
  }

  def isEvaluated: Boolean = option.isDefined
}

object Lazy {
  def lazily[T](thunk: => T): Lazy[T] = new Lazy(thunk)
}
