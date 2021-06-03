package moe.lisa.util.macros

import scala.quoted.*

object Position {
  def positionImpl(using Quotes): Expr[Int] =
    val pos = quotes.reflect.Position.ofMacroExpansion
    Expr(pos.start)
}
