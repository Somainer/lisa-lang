package moe.lisa.lang

import java.util.NoSuchElementException

trait ILookupable:
  def lookupOption(v: Any): Option[Any]
  def lookup(v: Any): Any =
    lookupOption(v).getOrElse(throw new NoSuchElementException(v.toString))
  def lookup(v: Any, default: Any): Any =
    lookupOption(v).getOrElse(default)
