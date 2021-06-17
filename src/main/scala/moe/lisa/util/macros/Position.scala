package moe.lisa.util.macros

import scala.quoted.*

object Position {
  def getPosition(using q: Quotes): q.reflect.Position =
    quotes.reflect.Position.ofMacroExpansion
}
