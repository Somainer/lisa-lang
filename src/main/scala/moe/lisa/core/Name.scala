package moe.lisa.core

import moe.lisa.lang.INamed

case class Name(name: String) extends INamed:
  override def namespace: String = ""
