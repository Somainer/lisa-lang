package moe.lisa.compile.transform

import scala.util.FromDigits
import com.somainer.nameof.NameOf._
import moe.lisa.core.Phases.Phase
import moe.lisa.core.context.Context
import moe.lisa.core.context.Context._
import moe.lisa.core.expression.{Constant, UntypedTree}
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Tree.Symbol
import moe.lisa.core.exceptions.MessageLisaSyntaxError
import moe.lisa.lang.{Symbol => LSymbol}
import moe.lisa.parsing.ParseTimeTraverser
import moe.lisa.parsing.ParseTimeDepCollector

object NumberToConstant extends Phase, ParseTimeTraverser, MiniTransform {
  override def name: String = nameOf(NumberToConstant)
  override def run(using Context): Unit =
    val unit = ctx.compilationUnit
    unit.untypedTree = numberToConstant(unit.untypedTree)

  def lit[T](tree: Tree)(v: String)(using fd: FromDigits[T]): Literal =
    cpy.Literal(tree)(Constant(fd.fromDigits(v)))
  def lit[T](tree: Tree)(v: String, radix: Int)(using wr: FromDigits.WithRadix[T]): Literal =
    cpy.Literal(tree)(Constant(wr.fromDigits(v, radix)))

  def numberToConstant(tree: Tree): Tree = traverse(tree) {
    case num @ Number(digits, kind) =>
      kind match
        case NumberKind.Whole(radix) => cpy.Literal(num)(Constant(FromDigits.intFromDigits(digits, radix)))
        case NumberKind.Long(radix) => cpy.Literal(num)(Constant(FromDigits.longFromDigits(digits, radix)))
        case NumberKind.BigNumber(radix) => lit[BigInt](num)(digits, radix)
        case NumberKind.Floating => cpy.Literal(num)(Constant(FromDigits.floatFromDigits(digits)))
        case NumberKind.Double => cpy.Literal(num)(Constant(FromDigits.doubleFromDigits(digits)))
        case NumberKind.BigDecimal => lit[BigDecimal](num)(digits)
  }

  override def transform(tree: Tree): Tree =
    try
      numberToConstant(tree)
    catch
      case n: NumberFormatException =>
        throw MessageLisaSyntaxError(n.getMessage, tree)
}
