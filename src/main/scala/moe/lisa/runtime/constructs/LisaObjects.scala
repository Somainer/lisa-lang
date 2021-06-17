package moe.lisa.runtime.constructs

import moe.lisa.core.Name
import moe.lisa.core.expression.Tree._
import moe.lisa.lang.Atom

object LisaObjects {
  def atom(s: String): Atom = Atom(s)

  def listFromAst(ast: Tree[?]): Any = ast match
    case Symbol(s) => s
    case LisaList(values) => values.map(listFromAst)
    case Literal(c) => c.value
    case x => x
    
  def bigInt(digits: String): BigInt = BigInt(digits)
  def bigDecimal(digits: String): BigDecimal = BigDecimal(digits)
}
