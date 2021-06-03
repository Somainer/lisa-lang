package moe.lisa.core.expression

import moe.lisa.core.expression.Tree._

trait ToLisaList[T >: Tree.Untyped] extends TreeInstance[T] {
  private object cpy extends TreeCopier[T]:
    override def postProcess(tree: Tree, copied: UntypedTree.Tree): copied.SelfType[T] =
      copied.asInstanceOf[copied.SelfType[T]]

  def toLisaList(tree: Tree): Tree = tree match
    case s @ Symbol(_) => s
    case LisaList(values) => cpy.LisaList(tree)(values.mapConserve(toLisaList))
    case RecordLiteral(values) => cpy.RecordLiteral(tree)(values.mapConserve(toLisaList))
    case If(condition, consequence, alternative) =>
      cpy.LisaList(tree)(List(cpy.Symbol(condition)("if"), toLisaList(condition), toLisaList(consequence), toLisaList(alternative)))
    case Apply(proc, args) =>
      cpy.LisaList(tree)(proc :: args)
    case Spread(tree) =>
      cpy.LisaList(tree)(List(cpy.Symbol(tree)("..."), tree))
    case LambdaExpression(CaseDef(pattern, guard, body)) =>
      // TODO: Add guard
      cpy.LisaList(tree)(List(
        cpy.Symbol(tree)("lambda"),
        toLisaList(pattern),
      ) ::: toLisaList(body).toTreeList)
    case t => t
}

object ToLisaList extends ToLisaList[?]
