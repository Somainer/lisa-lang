package moe.lisa.util

import moe.lisa.core.exceptions.ArityException
import scala.quoted._

object ThrowHelper:
  def unsupported(): Nothing = unsupported(null)
  def unsupported(message: String): Nothing =
    throw new UnsupportedOperationException(message)

  def throwArity(actual: Int, name: String): Nothing =
    throw ArityException(name, actual)

  inline def unreachable(): Nothing = ${ unreachableImpl }
  private def unreachableImpl(using Quotes): Expr[Nothing] =
    val position = macros.Position.getPosition
    val sourceFile = position.sourceFile
    val path = sourceFile.jpath
    val fileName = path.toFile.getName
    val message = s"Unreachable code executed in $fileName(${position.startLine + 1}:${position.startColumn + 1})"
    val messageExpr = Expr(message)
    '{ throw new Error($messageExpr) }
