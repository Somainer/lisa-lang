package moe.lisa.compile.transform

import com.somainer.nameof.NameOf._
import moe.lisa.core.Phases.Phase
import moe.lisa.core.context.Context
import moe.lisa.core.context.Context._
import moe.lisa.core.SourceFile.{NoSourceFile, SourceFile}
import moe.lisa.core.exceptions.MessageLisaSyntaxError
import moe.lisa.core.expression.Constant
import moe.lisa.core.expression.Properties._
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.UntypedTree
import moe.lisa.lang.{Atom, Symbol => LSymbol}
import moe.lisa.parsing.ParseTimeTraverser
import moe.lisa.parsing.ParseTimeDepCollector
import moe.lisa.util.ThrowHelper

object GenAst extends Phase, MiniTransform {
  override def name: String = nameOf(GenAst)
  override def run(using Context): Unit =
    val unit = ctx.compilationUnit
    unit.untypedTree = transform(unit.untypedTree)

  val basicPipeline = SpreadOp >> LambdaLiteralTransform >> NumberToConstant
  val bodyPipeline =
    ApplyStringTemplate

  def nilObj = LisaList(Nil)(using NoSourceFile)
  def nilObjWhere(sourceFile: SourceFile) = LisaList(Nil)(using sourceFile)
  def andSym = Symbol(LSymbol("and"))(using NoSourceFile)
  def True = Literal(Constant(true))(using NoSourceFile)
  def genCase(params: Tree, body: List[Tree])(outer: Tree): CaseDef =
    val bodyBlock = body match
      case Nil =>
        throw MessageLisaSyntaxError("body cannot be empty", outer)
      case expr :: Nil => expr
      case block :+ returnValue =>
        cpy.Block(ThicketOf(body))(block, returnValue)
      case _ => ThrowHelper.unreachable()
    val (pattern, guard) = params match
      // (pat (if a b c)) => CaseDef(pat, (and a b c))
      case LisaList(xs :+ (pp @ LisaList(Symbol(LSymbol("if")) :: pats))) =>
        val guard = pats match
          case Nil => throw MessageLisaSyntaxError("guard expects 1 predicate", pp)
          case x :: Nil => x
          case _ => cpy.Apply(ThicketOf(pats))(andSym, pats)
        (cpy.LisaList(params)(xs), guard)
      case lls @ LisaList(_) => (lls, EmptyTree)
      case sym @ Symbol(_) => (sym, EmptyTree)
      case _ => throw MessageLisaSyntaxError(s"Parameters can only be lists or symbols, but $params found.", params)
    cpy.CaseDef(ThicketOf(params :: body))(pattern, guard, bodyBlock)

  /**
   * Convert dot symbols to select.
   * foo.bar.baz => (.baz (.bar foo))
   * @param tree The symbol.
   * @return Tree
   */
  def desugarDotSymbol(tree: Symbol): Tree =
    if tree.name.fullName.startsWith(".") then tree
    else
      val name = tree.name.fullName
      val span = tree.span
      var start = span.start
      val selects = name.split('.')
      var init: Tree = cpy.Symbol(tree)(selects.head).withSpan {
        val length = selects.head.length
        start += length + 1
        span.withEnd(start - 1)
      }
      for sel <- selects.tail do
        val len = sel.length
        // last.current.next
        // ^^^^^^^^^^^^
        val totalEnd = start + len
        init = cpy.Select(tree)(init, cpy.Symbol(tree)(sel).withSpan {
          span.withStart(start).withEnd(totalEnd)
        }).withSpan(span.withEnd(totalEnd))
        start = totalEnd + 1
      init

  private def genAst(tree: Tree): Tree = tree match
    case ls@LisaList(values) if ls.kind.isSquare =>
      cpy.LisaList(ls)(values.map(genAst))
    case LisaList(values) =>
      values match
        case Nil => nilObjWhere(tree.source).withSpan(tree.span)
        case Symbol(LSymbol("define")) :: (sym @ (Symbol(_) | Typed(_, _))) :: value :: Nil =>
          cpy.Define(tree)(sym, genAstBody(value))
        case Symbol(LSymbol("define")) :: LisaList(sym :: params) :: body =>
          val genParams = params.map(genAst)
          val genBody = genAstBody(body)
          cpy.Define(tree)(genAst(sym), cpy.LambdaExpression(tree)(genCase(LisaList(genParams)(using NoSourceFile), genBody)(tree)))
        case Symbol(LSymbol("define-macro")) :: LisaList((sym @ Symbol(_)) :: params) :: body =>
          val genBody = genAstBody(body)
          val lambdaExpression = cpy.LambdaExpression(tree)(genCase(LisaList(params.map(genAst))(using NoSourceFile), genBody)(tree))
          cpy.Define(tree)(sym, lambdaExpression.addMarker(Macro))
        case Symbol(LSymbol("lambda" | "λ")) :: xs =>
          val caseDef = xs match {
            case Nil | _ :: Nil => throw MessageLisaSyntaxError("lambda body can not be empty", tree)
            case (p @ LisaList(pat)) :: xs =>
              genCase(cpy.LisaList(p)(pat.map(genAst)), genAstBody(xs))(tree)
            case x :: xs =>
              // TODO: Add (lambda x x)
              throw MessageLisaSyntaxError("unknown lambda parameters", x)
          }
          cpy.LambdaExpression(tree)(caseDef)
        case obj :: Literal(Constant(Atom("typed"))) :: tpe :: Nil =>
          cpy.Typed(tree)(obj, tpe)
        case Symbol(LSymbol("if")) :: xs => tree // Keep unchanged because if can never be variables.
        case (s @ Symbol(LSymbol(sym))) :: obj :: args if sym.startsWith(".") =>
          val symbol = cpy.Symbol(s)(sym.substring(1))
          val qual = genAstBody(obj)
          if args.isEmpty then cpy.Select(tree)(qual, symbol)
          else cpy.Apply(tree)(cpy.Select(s)(qual, symbol), genAstBody(args))
        case proc :: args => cpy.Apply(tree)(genAstBody(proc), genAstBody(args))
    case PackageDef(pid, stats) =>
      cpy.PackageDef(tree)(pid, stats.map(genAst))
    case Spread(toSp) =>
      cpy.Spread(tree)(transform(toSp))
    case Define(sym, value) =>
      cpy.Define(tree)(transform(sym), genAstBody(value))
    case sym @ Symbol(LSymbol(name)) if !sym.isBackQuoted && name.contains(".") =>
      desugarDotSymbol(sym)
    case _ => transformBody(tree)
  
  private def genAstBody(tree: Tree): Tree = transformBody(tree) match {
    case ll @ LisaList(values) if ll.kind.isRound =>
      values match {
        case Symbol(LSymbol("if")) :: xs => xs match
          case pred :: cons :: alt :: Nil =>
            cpy.If(tree)(genAstBody(pred), genAstBody(cons), genAstBody(alt))
          case pred :: cons :: Nil =>
            cpy.If(tree)(genAstBody(pred), genAstBody(cons), EmptyTree)
          case _ => throw MessageLisaSyntaxError("if expect 2 or 3 expressions.", tree)
        case _ => genAst(ll) // Non-body pattern
      }
    case tree => genAst(tree)
  }
  private def genAstBody(trees: List[Tree]): List[Tree] = trees.map(genAstBody)
  def transformBody(tree: Tree): Tree = bodyPipeline.transform(tree)

  override def transform(tree: Tree): Tree = genAst(basicPipeline.transform(tree))
  def transformBodyExpression(tree: Tree): Tree = genAstBody(tree)
}
