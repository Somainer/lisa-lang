package moe.lisa.compile.transform

import com.somainer.nameof.NameOf._
import moe.lisa.core.Phases.Phase
import moe.lisa.core.context.Context
import moe.lisa.core.context.Context._
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Tree.Symbol
import moe.lisa.lang.{Symbol as LSymbol}
import moe.lisa.parsing.ParseTimeTraverser
import moe.lisa.parsing.ParseTimeDepCollector

object LambdaLiteralTransform extends Phase with ParseTimeTraverser {
  override def name: String = nameOf(LambdaLiteralTransform)

  override def run(using Context): Unit =
    val unit = ctx.compilationUnit
    unit.untypedTree = lambdaLiteral(unit.untypedTree)

  def lambdaLiteral(tree: Tree): Tree = traverse(tree) {
    case ll @ LambdaLiteral(body) =>
      val transformed = lambdaLiteral(body)
      val deps = ParseTimeDepCollector.collect(transformed)
      val args = deps.toSeq.filter(_.name.fullName.startsWith("#")).sortBy {
        case Symbol(LSymbol("#")) => -1
        case Symbol(LSymbol(s"#$d")) => d.toInt
      }
      val realArguments = 0.until(args.length).map(id => cpy.Symbol(args(id))(LSymbol(s"arg$id"))).toList
      val replacements = args.zip(realArguments).toMap
      val replacedBody = traverse(transformed) {
        case sym @ Symbol(_) if replacements.contains(sym) => replacements(sym)
      }
      cpy.LambdaExpression(ll)(cpy.CaseDef(ll)(cpy.LisaList(ll)(realArguments), EmptyTree, replacedBody))
  }
}
