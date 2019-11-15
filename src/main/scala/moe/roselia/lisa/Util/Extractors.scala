package moe.roselia.lisa.Util

object Extractors {
  object NeedInt {
    def apply(s: Int): String = s.toString

    def unapply(arg: String): Option[Int] = arg.toIntOption
  }
}
