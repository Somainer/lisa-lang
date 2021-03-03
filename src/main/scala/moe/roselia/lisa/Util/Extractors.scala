package moe.roselia.lisa.Util

import moe.roselia.lisa.LispExp.{Expression, WithSourceTree}

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

  implicit class SourceExtension(val source: Expression) extends AnyVal {
    def sourceLineContent: String =
      source.sourceTree.map(_.location.lineContents.stripLeading()).getOrElse(source.code)
  }
}
