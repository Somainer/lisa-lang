package moe.lisa.core.expression

import Tree._
import moe.lisa.lang.{Symbol as LSymbol}

object ShowExpr {
  def showSource(tree: Tree[?]): String =
    val span = tree.span
    val content = new String(tree.source.content())
    content.substring(span.start, span.end)

  def show(tree: Tree[?]): String = tree match
    case Symbol(LSymbol(s)) => s
    case l @ LisaList(ll) =>
      if l.kind.isRound then
        ll.map(show).mkString("(", " ", ")")
      else ll.map(show).mkString("[", " ", "]")
    case RecordLiteral(values) =>
      values.map(show).mkString("{", " ", "}")
    case Literal(const) => const.stringValue
    case x => x.toString
  def showIndented(tree: Tree[?])(margin: Int = 2)(index: Int = 0): String =
    val indent = " ".repeat(margin * index)
    tree match
      case l @ LisaList(values) if values.length > 4 && l.kind.isRound =>
        val first :: second :: tail = values.map(showIndented(_)(margin)(index + 1)): @unchecked
        val rest = tail.mkString("\n")
        val firstPart = first.stripLeading()
        val secondPart = second.stripLeading()
        s"$indent($firstPart $secondPart\n$rest)"
      case l @ LisaList(values) if values.length > 4 && l.kind.isSquare =>
        val body = values.map(showIndented(_)(margin)(index + 1)).mkString("\n")
        s"$indent[\n$body\n$indent]"
      case RecordLiteral(values) if values.length > 4 =>
        val body = values.grouped(2).map {
          case h :: t :: Nil =>
            showIndented(h)(margin)(index + 1) + " " + showIndented(t)(margin)(index + 1).stripLeading()
          case h :: Nil =>
            showIndented(h)(margin)(index + 1)
          case _ => ??? // Make dotc happy.
        }.mkString("\n")
        s"$indent{\n$body\n$indent}"
      case t => indent + show(tree)
}
