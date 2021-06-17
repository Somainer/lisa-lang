package moe.lisa.compile

import dotty.tools.dotc.ast.{untpd => scala}
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Constants.{Constant => SConstant}
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Definitions
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.report
import scala.{Ident, TypeTree, cpy => scpy}
import moe.lisa.core.expression.Constant
import moe.lisa.core.expression.{Tree => lisa}
import moe.lisa.core.expression.{UntypedTree => lisaUtpd}
import moe.lisa.lang.{Symbol => LSymbol}
import com.somainer.nameof.NameOf._
import moe.lisa.core.exceptions.MessageLisaSyntaxError
import moe.lisa.util.DottyContext

class GenScalaCode(using Context) {
  extension(tree: lisaUtpd.Tree)
    def dummy: scala.Tree =
      val sTree = scala.Literal(SConstant(null))(using tree.source)
      sTree.withSpan(tree.span)
  def emptyScalaTree = scala.EmptyTree
  val defn: Definitions = ctx.definitions

  def toSelect(xs: String): scala.Tree =
    val parts = xs.split('.')
    val symbol = parts.head
    val selects = parts.tail
    selects.foldLeft[scala.Tree](scala.Ident(termName(symbol)))((t, n) => scala.Select(t, termName(n)))
  val scalaPackage = toSelect("scala")
  val runtimeConstruct = toSelect(qualifiedNameOfType[moe.lisa.runtime.constructs.LisaObjects.type].stripSuffix("$"))
  val bigInt: scala.Tree = scala.Select(runtimeConstruct, termName(nameOf(bigInt)))
  val bigDecimal: scala.Tree = scala.Select(runtimeConstruct, termName(nameOf(bigDecimal)))
  val atom: scala.Tree = scala.Select(runtimeConstruct, termName(nameOf(atom)))
  val list: scala.Tree = scala.Select(scalaPackage, termName(nameOf(List)))
  val cons: scala.Tree = scala.Select(scalaPackage, termName(nameOf(::)))
  val concatSymbol = termName(nameOf(Nil.++))
  def concatList(la: scala.Tree, lb: scala.Tree) =
    scpy.Apply(lb)(scpy.Select(la)(la, concatSymbol), lb :: Nil)

  def transformList(tree: lisaUtpd.LisaList): scala.Tree =
    import collection.mutable.ListBuffer
    assert(tree.kind.isSquare)
    val segments = ListBuffer.empty[List[lisaUtpd.Tree]]
    val buf = ListBuffer.empty[lisaUtpd.Tree]
    for t <- tree.values do
      t match
        case lisa.Spread(sp) =>
          buf += sp
          segments += buf.result()
          buf.clear()
        case t => buf += t

    def genFromApply(values: List[lisaUtpd.Tree]) = scpy.Apply(tree.dummy)(list, values.map(transform))
    def genSegment(values: List[lisaUtpd.Tree]) =
      val init :+ last = values.map(transform): @unchecked
      init.foldRight(last) { (x, xs) =>
        scpy.Apply(xs)(cons, x :: xs :: Nil)
      }
    end genSegment
    def concatSegments(segs: Seq[scala.Tree]) = segs.reduce(concatList)

    val bufElements = genFromApply(buf.result())
    if segments.isEmpty then
      bufElements
    else
      val segmentCadidates = segments.result().map(genSegment)
      val candidates =
        if buf.isEmpty then segmentCadidates
        else segmentCadidates :+ bufElements
      concatSegments(candidates)
  end transformList


  def convertToType(from: lisaUtpd.Tree)(tree: scala.Tree): scala.Tree = tree match
    case scala.Ident(name) => scpy.Ident(tree)(name.toTypeName)
    case _ =>
      throw MessageLisaSyntaxError("Can not convert to type", from)

  def makeParamClause(param: lisaUtpd.Tree, allowUntyped: Boolean = false): scala.ValDef = param match {
    case tp @ lisa.Symbol(LSymbol(name)) if allowUntyped =>
      scpy.ValDef(tp.dummy)(termName(name), TypeTree(), emptyScalaTree)
        .withMods(scala.EmptyModifiers.withFlags(Flags.Param))
    case tp @ lisa.Typed(lisa.Symbol(LSymbol(name)), tpt) =>
      scpy.ValDef(tp.dummy)(termName(name), convertToType(tpt)(transform(tpt)), emptyScalaTree)
      .withMods(scala.EmptyModifiers.withFlags(Flags.Param))
    case tp @ lisa.Typed(lisa.Spread(lisa.Symbol(LSymbol(name))), tpt) =>
      val typeTree = convertToType(tpt)(transform(tpt))
      scpy.ValDef(tp.dummy)(termName(name), scpy.PostfixOp(tpt.dummy)(typeTree, Ident(nme.*.toTypeName)), emptyScalaTree)
        .withMods(scala.EmptyModifiers.withFlags(Flags.Param))

    case tree => throw MessageLisaSyntaxError("Can not infer type of parameter.", tree)
  }

  def makeParamsClauses(params: List[lisaUtpd.Tree], allowUntyped: Boolean = false): scala.ParamClause =
    params.map(makeParamClause(_, allowUntyped))

  def transformDefine(tree: lisaUtpd.Define): scala.Tree =
    val (name, tpt) = tree.sym match
      case lisa.Symbol(LSymbol(sym)) =>
        (termName(sym), scala.TypeTree())
      case lisa.Typed(s @ lisa.Symbol(LSymbol(sym)), tpt) =>
        (termName(sym), convertToType(tpt)(transform(tpt)))
    tree.value match
      case lisa.LambdaExpression(definition) =>
        if !definition.guard.hasNoChild
        then throw MessageLisaSyntaxError("Can not reduce guard.", definition.guard)
        definition.pattern match
          case lisa.LisaList(pat) =>
            val paramsClauses =
              if pat.isEmpty then Nil else List(makeParamsClauses(pat))
            scpy.DefDef(tree.dummy)(name, paramsClauses, tpt, transform(definition.body))
              .withMods(scala.EmptyModifiers.withFlags(Flags.Method))
          case p =>
            throw MessageLisaSyntaxError("Can not reduce parameters to lists.", p)
      case _ =>
        val rhs = transform(tree.value)
        scpy.ValDef(tree.dummy)(name, tpt, rhs)

  def transform(tree: lisaUtpd.Tree): scala.Tree = tree match
    case lisa.Thicket(Nil) => scala.EmptyTree
    case lisa.Symbol(LSymbol(sym)) => scpy.Ident(tree.dummy)(termName(sym))
    case define @ lisa.Define(_, _) =>
      transformDefine(define)
    case lisa.Literal(const) =>
      const.tag match
        case Constant.BigIntTag => scpy.Apply(tree.dummy)(bigInt, scpy.Literal(tree.dummy)(SConstant(const.bigIntValue.toString())) :: Nil)
        case Constant.BigDecimalTag => scpy.Apply(tree.dummy)(bigDecimal, scpy.Literal(tree.dummy)(SConstant(const.bigDecimalValue.toString())) :: Nil)
        case Constant.AtomTag => scpy.Apply(tree.dummy)(atom, scala.Literal(SConstant(const.atomValue.value)) :: Nil)
        case _ => scpy.Literal(tree.dummy)(SConstant(const.value))
    case lisa.Select(qualifier, lisa.Symbol(LSymbol(name))) =>
      scpy.Select(tree.dummy)(transform(qualifier), termName(name))
    case lisa.Apply(proc, args) =>
      scpy.Apply(tree.dummy)(transform(proc), args.map(transform))
    case lisa.Block(stats, expr) =>
      scpy.Block(tree.dummy)(stats.map(transform), transform(expr))
    case lisa.If(condition, consequence, alternative) =>
      scpy.If(tree.dummy)(transform(condition), transform(consequence), transform(alternative))
    case lisa.Spread(expr) =>
      val toSpred = transform(expr)
      scpy.Typed(tree.dummy)(toSpred, scala.Ident(nme.WILDCARD_STAR.toTypeName))
    case ls @ lisa.LisaList(_) if ls.kind.isSquare =>
      transformList(ls)
    case lisa.LisaList(Nil) => scpy.Tuple(tree.dummy)(Nil)
    case record @ lisa.RecordLiteral(_) => transformRecord(record)
    case lisa.LambdaExpression(definition) =>
      if !definition.guard.hasNoChild then
        throw MessageLisaSyntaxError("can not reduce guard in lambda expression", definition.guard)
      definition.pattern match
        case lisa.LisaList(pattern) =>
          val pat = makeParamsClauses(pattern, allowUntyped = true)
          scpy.Function(tree.dummy)(pat, transform(definition.body))
        case _ =>
          throw MessageLisaSyntaxError(s"Function parameters can only be lists.", definition.pattern)
    case _ => throw MessageLisaSyntaxError(s"Not reduced enough $tree", tree)

  def transformPackage(pkg: lisaUtpd.PackageDef): scala.PackageDef =
    val name = 
      if pkg.pid.name.fullName.isEmpty then 
        scala.Ident(nme.EMPTY_PACKAGE)
      else
        scpy.Ident(pkg.pid.dummy)(termName(pkg.pid.name.fullName))
    val stats = pkg.stats.map(transform)
    scpy.PackageDef(pkg.dummy)(name, defaultImports ::: stats)
    
  def apply(tree: lisaUtpd.Tree): scala.Tree = tree match {
    case pkg: lisaUtpd.PackageDef => transformPackage(pkg)
    case _ => transform(tree)
  }

  def predefPackage = toSelect(qualifiedNameOfType[moe.lisa.lang.Predef])
  def dynamicPredefPackage = toSelect(qualifiedNameOfType[moe.lisa.lang.DynamicPredef])
  def wildCardSelectImport(sel: scala.Tree) = scala.Import(sel, List(scala.ImportSelector(scala.Ident(nme.WILDCARD))))
  lazy val defaultImports = List(
    wildCardSelectImport(predefPackage),
//    wildCardSelectImport(dynamicPredefPackage)
  )

  import moe.lisa.lang.LisaRecord
  lazy val emptyRecord = scala.Select(toSelect(qualifiedNameOfType[LisaRecord]), termName(nameOf(LisaRecord.empty)))
  def transformRecord(rec: lisaUtpd.RecordLiteral): scala.Tree =
    def recur(recs: List[lisaUtpd.Tree], curr: scala.Tree): scala.Tree = recs match {
      case Nil => curr
      case lisa.Spread(record) :: xs =>
        val application = scpy.Apply(lisaUtpd.Thicket(recs).dummy)(
          scpy.Select(record.dummy)(curr, termName(nameOf[LisaRecord](_.updatedMany))),
          transform(record) :: Nil
        )
        recur(xs, application)
      case key :: value :: xs =>
        val application = scpy.Apply(lisaUtpd.Thicket(recs).dummy)(
          scpy.Select(key.dummy)(curr, termName(nameOf[LisaRecord](_.updatedOne(???, ???)))),
          transform(key) :: transform(value) :: Nil
        )
        recur(xs, application)
      case key :: Nil =>
        throw MessageLisaSyntaxError("can not convert to record", key)
    }
    recur(rec.values, emptyRecord)
}

object GenScalaCode extends GenScalaCode(using DottyContext.ctx)
