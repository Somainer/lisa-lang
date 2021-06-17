package moe.lisa.parsing

import moe.lisa.core.expression.UntypedTree._

trait ParseTimeAccumulator[X] {
  def apply(x: X, tree: Tree): X
  def apply(x: X, trees: List[Tree]): X =
    trees.foldLeft(x)(apply)

  def foldOver(x: X, tree: Tree): X = tree match
    case LisaList(values) => this(x, values)
    case PackageDef(pid, stats) =>
      stats.foldLeft(x)(this(_, _))
    case StringTemplate(templateName, parts, arguments) =>
      arguments.foldLeft(x)(this(_, _))
    case RecordLiteral(values) => this(x, values)
    case q @ Quote(expr) if q.quasi => this(x, expr)
    case uq @ UnQuote(expr) => this(x, expr)
    case _ => this(x, tree)
}

object ParseTimeDepCollector extends ParseTimeAccumulator[Set[Symbol]] {
  override def apply(x: Set[Symbol], tree: Tree): Set[Symbol] = tree match
    case s: Symbol => x + s
    case _ => x

  def collect(tree: Tree): Set[Symbol] = this.foldOver(Set.empty, tree)
}
