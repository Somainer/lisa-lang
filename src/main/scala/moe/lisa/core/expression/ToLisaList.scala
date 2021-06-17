package moe.lisa.core.expression

import moe.lisa.core.expression.Tree._

trait ToLisaList[T >: Tree.Untyped] extends TreeInstance[T] {
  private object cpy extends TreeCopier[T]:
    override def postProcess(tree: Tree, copied: UntypedTree.Tree): copied.SelfType[T] =
      copied.asInstanceOf[copied.SelfType[T]]

  def toLisaList(tree: Tree): Tree = tree match
    case t if t.sourceLisaList.isDefined => t.sourceLisaList.get
    case n if n.hasNoChild => n
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
      val guardList = toLisaList(guard)
      val patList = toLisaList(pattern) match {
        case l @ LisaList(values) => cpy.LisaList(l)(values ::: {
          if guardList.hasNoChild then Nil
          else List(cpy.LisaList(guard)(List(cpy.Symbol(guard)("if"), guardList)))
        })
      }
      cpy.LisaList(tree)(List(
        cpy.Symbol(tree)("lambda"),
        patList,
      ) ::: toLisaList(body).toTreeList)
    case t => t
    
  def toValueList(tree: Tree): Any = tree match {
    case Literal(const) => const.value
    case Symbol(sym) => sym
    case LisaList(values) => values.map(toLisaList)
    case x => x
  }
}

object ToLisaList extends ToLisaList[?]:
  def of[T >: Untyped] = new ToLisaList[T] {}
