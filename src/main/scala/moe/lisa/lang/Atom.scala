package moe.lisa.lang

import moe.lisa.core.Name
import moe.lisa.util.StringLiteral

case class Atom(value: String) extends IDynamicInvokable:
  lazy val valuePart =
    val value = name
    if value matches "\\S+" then value
    else StringLiteral.toLiteralCode(value)
  def apply(r: ILookupable): Any = r.lookup(this)

  def name: String = value

  override def toString: String =
    s":$valuePart"

object Atom:
  def apply(name: String) = new Atom(name)
