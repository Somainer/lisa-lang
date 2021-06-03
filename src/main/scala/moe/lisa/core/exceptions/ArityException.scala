package moe.lisa.core.exceptions

case class ArityException(name: String, actual: Int) 
  extends IllegalArgumentException(s"Wrong number of arguments ($actual) passed to $name")
