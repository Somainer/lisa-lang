package moe.roselia.lisa.Typing

sealed trait TypeVariance
object TypeVariance {
  object Invariant extends TypeVariance
  object Covariant extends TypeVariance
  object Contravariant extends TypeVariance
}
