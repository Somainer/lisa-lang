package moe.lisa.core.expression

import moe.lisa.core.SourceFile.SourceFile
import moe.lisa.lang.{Symbol => LSymbol}

trait TreeCopier[T >: Tree.Untyped] extends TreeInstance[T] {
  def postProcess(tree: Tree, copied: UntypedTree.Tree): copied.SelfType[T]

  inline def sourceFile(tree: Tree): SourceFile = tree.source

  def finalize(tree: Tree, copied: UntypedTree.Tree): copied.SelfType[T] =
    postProcess(tree, copied.withSpan(tree.span).withAttachmentsFrom(tree))

  def Symbol(tree: Tree)(name: LSymbol): Symbol = tree match {
    case tree: Symbol if name == tree.name => tree
    case _ => finalize(tree, UntypedTree.Symbol(name)(using sourceFile(tree)))
  }
  def Symbol(tree: Tree)(name: String): Symbol = Symbol(tree)(LSymbol(name))
  def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree): If = tree match {
    case tree: If if (cond eq tree.condition) && (thenp eq tree.consequence) && (elsep eq tree.alternative) => tree
    case _ => finalize(tree, UntypedTree.If(cond, thenp, elsep)(using sourceFile(tree)))
  }
  def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree): CaseDef = tree match {
    case tree: CaseDef if (pat eq tree.pattern) && (guard eq tree.guard) && (body eq tree.body) => tree
    case _ => finalize(tree, UntypedTree.CaseDef(pat, guard, body)(using sourceFile(tree)))
  }
  def Match(tree: Tree)(selector: Tree, cases: List[CaseDef]): Match = tree match {
    case tree: Match if (selector eq tree.selector) && (cases eq tree.cases) => tree
    case _ => finalize(tree, UntypedTree.Match(selector, cases)(using sourceFile(tree)))
  }
  def Block(tree: Tree)(stats: List[Tree], expr: Tree): Block = tree match {
    case tree: Block if (stats eq tree.stats) && (expr eq tree.expr) => tree
    case _ => finalize(tree, UntypedTree.Block(stats, expr)(using sourceFile(tree)))
  }
  def LisaList(tree: Tree)(values: List[Tree]): LisaList = tree match {
    case tree: LisaList if tree.values eq values => tree
    case _ => finalize(tree, UntypedTree.LisaList(values)(using sourceFile(tree)))
  }
  def Try(tree: Tree)(expr: Tree, cases: List[CaseDef], finalizer: Tree): Try = tree match {
    case tree: Try if (expr eq tree.expr) && (cases eq tree.cases) && (finalizer eq tree.finalizer) => tree
    case _ => finalize(tree, UntypedTree.Try(expr, cases, finalizer)(using sourceFile(tree)))
  }
  def Quote(tree: Tree)(expr: Tree): Quote = tree match {
    case tree: Quote if tree.expr eq expr => tree
    case _ => finalize(tree, UntypedTree.Quote(expr)(using sourceFile(tree)))
  }
  def UnQuote(tree: Tree)(expr: Tree): UnQuote = tree match {
    case tree: UnQuote if tree.expr eq expr => tree
    case _ => finalize(tree, UntypedTree.UnQuote(expr)(using sourceFile(tree)))
  }
  def Literal(tree: Tree)(const: Constant): Literal = tree match {
    case tree: Literal if tree.const eq const => tree
    case _ => finalize(tree, UntypedTree.Literal(const)(using sourceFile(tree)))
  }
  def Define(tree: Tree)(sym: Tree, value: Tree) = tree match {
    case tree: Define if (tree.sym eq sym) && (tree.value eq value) => tree
    case _ => finalize(tree, UntypedTree.Define(sym, value)(using sourceFile(tree)))
  }
  def LambdaExpression(tree: Tree)(definition: CaseDef) = tree match {
    case tree: LambdaExpression if tree.definition eq definition => tree
    case _ => finalize(tree, UntypedTree.LambdaExpression(definition)(using sourceFile(tree)))
  }
  def RecordLiteral(tree: Tree)(values: List[Tree]) = tree match {
    case tree: RecordLiteral if tree.values eq values => tree
    case _ => finalize(tree, UntypedTree.RecordLiteral(values)(using sourceFile(tree)))
  }
  def Apply(tree: Tree)(proc: Tree, args: List[Tree]): Apply = tree match {
    case tree: Apply if (proc eq tree.proc) && (args eq tree.args) => tree
    case _ => finalize(tree, UntypedTree.Apply(proc, args)(using sourceFile(tree)))
  }
  def Spread(tree: Tree)(t: Tree): Spread = tree match {
    case tree: Spread if t eq tree.tree => tree
    case _ => finalize(tree, UntypedTree.Spread(t)(using sourceFile(tree)))
  }
  def Select(tree: Tree)(qualifier: Tree, name: Symbol): Select = tree match {
    case tree: Select if (qualifier eq tree.qualifier) && (name == tree.name) => tree
    case _ => finalize(tree, UntypedTree.Select(qualifier, name)(using sourceFile(tree)))
  }
  def Typed(tree: Tree)(expr: Tree, tpt: Tree): Typed = tree match {
    case tree: Typed if (expr eq tree.expr) && (tpt eq tree.tpt) => tree
    case tree => finalize(tree, UntypedTree.Typed(expr, tpt)(using sourceFile(tree)))
  }
  def Thicket(tree: Tree)(trees: List[Tree]): Thicket = tree match {
    case tree: Thicket if (trees eq tree.trees) => tree
    case _ => finalize(tree, UntypedTree.Thicket(trees)(using sourceFile(tree)))
  }
}
