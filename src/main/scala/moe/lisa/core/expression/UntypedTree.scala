package moe.lisa.core.expression
import moe.lisa.core.SourceFile._
import moe.lisa.core.expression.Tree.Untyped
import moe.lisa.core.expression.UntypedTree.UntypedTreeCopier
import moe.lisa.lang.{Symbol => LSymbol}

object UntypedTree extends TreeInstance[Tree.Untyped]:
  case class Number(digits: String, kind: NumberKind)(using SourceFile) extends Tree
  object Number:
    def infer(rawDigits: String, isFloating: Boolean, radix: Int = 10)(using SourceFile): Number =
      if rawDigits startsWith "-" then
        val nonNeg = infer(rawDigits.substring(1), isFloating, radix)
        return nonNeg.copy(s"-${nonNeg.digits}")(using nonNeg.source)
      if rawDigits startsWith "0x" then
        return infer(rawDigits.substring(2), isFloating, radix)
      if isFloating then
        rawDigits.last match
          case 'f' | 'F' => Number(rawDigits.init, NumberKind.Floating)
          case 'd' | 'D' => Number(rawDigits.init, NumberKind.Double)
          case 'm' | 'M' => Number(rawDigits.init, NumberKind.BigDecimal)
          case _ => Number(rawDigits, NumberKind.Double)
      else
        rawDigits.last match
          case 'l' | 'L' => Number(rawDigits.init, NumberKind.Long(radix))
          case 'n' | 'N' => Number(rawDigits.init, NumberKind.BigNumber(radix))
          case _ => Number(rawDigits, NumberKind.Whole(radix))

  enum NumberKind:
    case Whole(radix: Int)
    case Long(radix: Int)
    case BigNumber(radix: Int)
    case BigDecimal
    case Floating
    case Double

  case class LambdaLiteral(body: Tree)(using SourceFile) extends Tree
  case class StringTemplate(templateName: Symbol, parts: List[Literal], arguments: List[Tree])(using SourceFile) extends Tree

  class EmptyTree extends Thicket(Nil)(using NoSourceFile)
  object EmptyTree extends EmptyTree

  object UntypedTreeCopier extends TreeCopier[Tree.Untyped] {
    override def postProcess(tree: UntypedTreeCopier.Tree, copied: UntypedTree.Tree): copied.SelfType[Untyped] =
      copied.asInstanceOf[copied.SelfType[Untyped]]
  }

  import Tree._
  def Symbol(sym: LSymbol)(using SourceFile): Symbol = new Symbol(sym)
  def Symbol(sym: String)(using SourceFile): Symbol = Symbol(LSymbol(sym))
  def If(condition: Tree, consequence: Tree, alternative: Tree)(using SourceFile) = new If(condition, consequence, alternative)
  def CaseDef(pattern: Tree, guard: Tree, body: Tree)(using SourceFile) = new CaseDef(pattern, guard, body)
  def Match(selector: Tree, cases: List[CaseDef])(using SourceFile) = new Match(selector, cases)
  def Block(stats: List[Tree], expr: Tree)(using SourceFile) = new Block(stats, expr)
  def LisaList(values: List[Tree])(using SourceFile) = new LisaList(values)
  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree)(using SourceFile) = new Try(expr, cases, finalizer)
  def Quote(expr: Tree)(using SourceFile) = new Quote(expr)
  def UnQuote(expr: Tree)(using SourceFile) = new UnQuote(expr)
  def Literal(const: Constant)(using SourceFile) = new Literal(const)
  def Define(sym: Tree, value: Tree)(using SourceFile) = new Define(sym, value)
  def LambdaExpression(definition: CaseDef)(using SourceFile) = new LambdaExpression(definition)
  def RecordLiteral(values: List[Tree])(using SourceFile) = new RecordLiteral(values)
  def Apply(proc: Tree, args: List[Tree])(using SourceFile) = new Apply(proc, args)
  def Spread(tree: Tree)(using SourceFile) = new Spread(tree)
  def Select(qualifier: Tree, name: Symbol)(using SourceFile) = new Select(qualifier, name)
  def Typed(expr: Tree, tpt: Tree)(using SourceFile) = new Typed(expr, tpt)
  def Thicket(trees: List[Tree])(using SourceFile) = new Thicket(trees)
  def ThicketOf(trees: List[Tree]) = Thicket(trees)(using NoSourceFile)
  def PackageDef(pid: Symbol, trees: List[Tree])(using SourceFile): PackageDef = new PackageDef(pid, trees)
  def PackageDef(trees: List[Tree])(using SourceFile): PackageDef = PackageDef(Symbol("")(using NoSourceFile), trees)

export moe.lisa.core.expression.{UntypedTree => untpd}
