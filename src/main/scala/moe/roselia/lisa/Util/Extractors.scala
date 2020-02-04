package moe.roselia.lisa.Util

object Extractors {
  object NeedInt {
    def apply(s: Int): String = s.toString

    def unapply(arg: String): Option[Int] = arg.toIntOption
  }

  implicit class RichOption[T](op: Option[T]) {
    def getOrThrow[Ex <: Throwable](ex: => Ex): T = {
      op.getOrElse(throw ex)
    }
  }
}
