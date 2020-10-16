package moe.roselia.lisa.Exceptions

import moe.roselia.lisa.LispExp.Expression

case class LisaRuntimeException(source: Expression, cause: Exception) extends Exception(cause) with LisaException
