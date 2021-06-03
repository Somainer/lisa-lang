package moe.lisa.util

import moe.lisa.core.exceptions.ArityException

object ThrowHelper:
  def unsupported(): Nothing = unsupported(null)
  def unsupported(message: String): Nothing =
    throw new UnsupportedOperationException(message)

  def throwArity(actual: Int, name: String): Nothing =
    throw ArityException(name, actual)
