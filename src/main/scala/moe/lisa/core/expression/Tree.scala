package moe.lisa.core.expression

import scala.compiletime.uninitialized
import scala.annotation.constructorOnly
import dotty.tools.dotc.util.{Attachment, SourceFile, Spans}
import dotty.tools.dotc.ast.Positioned
import moe.lisa.core.Name
import moe.lisa.core.expression.Tree._
import moe.lisa.lang.{Symbol => LSymbol}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer


object Tree {
  import Properties._

  type Untyped = Null
  trait Tree[-T >: Untyped](using source: SourceFile) extends Positioned, Attachment.Container, FromLisaList[T]:
    type SelfType[T >: Untyped] <: Tree[Untyped]

    private var myType: T @uncheckedVariance = uninitialized

    def tpe: T @uncheckedVariance = {
      if myType == null then throw UnsupportedOperationException("")
      myType
    }

    private def overrideTypeUnchecked(tpe: LisaType): SelfType[LisaType] =
      val theTree =
        (if myType == null || (myType.asInstanceOf[AnyRef] eq tpe) then this
        else cloneIn(source)).asInstanceOf[Tree[LisaType]]
      theTree.myType = tpe
      theTree.asInstanceOf[SelfType[LisaType]]

    def isType: Boolean = false
    def isExpression: Boolean = false
    def toTreeList: List[Tree[T]] = this :: Nil
    def foreachInThicket(op: Tree[T] => Unit): Unit = op(this)
    def hasNoChild: Boolean = false
  end Tree

  trait FromLisaList[-T >: Untyped] {
    var sourceLisaList: Option[Tree[T]] @ uncheckedVariance = None
    def withSourceLisaList[B >: Untyped <: T](ll: Tree[B]): this.type = {
      sourceLisaList = Some(ll.asInstanceOf[Tree[T]])
      this
    }
  }

  case class Symbol[-T >: Untyped](name: LSymbol)(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = Symbol[T]
    def isBackQuoted: Boolean = hasAttachment(BackQuoted)
  
  case class If[-T >: Untyped]
    (condition: Tree[T], consequence: Tree[T], alternative: Tree[T])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = If[T]

  case class CaseDef[-T >: Untyped](pattern: Tree[T], guard: Tree[T], body: Tree[T])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = CaseDef[T]

  case class Match[-T >: Untyped](selector: Tree[T], cases: List[CaseDef[T]])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = Match[T]

  case class Block[-T >: Untyped](stats: List[Tree[T]], expr: Tree[T])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = Block[T]
  
  case class LisaList[-T >: Untyped](values: List[Tree[T]])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = LisaList[T]
//    override def apply(i: Int): Tree[T] = values(i)
//    override def iterator: Iterator[Tree[T]] = values.iterator
//    override def length: Int = values.length

    def kind: BracketKind = getAttachment(BracketType).getOrElse(BracketKind.Round)

  case class Try[-T >: Untyped](expr: Tree[T], cases: List[CaseDef[T]], finalizer: Tree[T])(using SourceFile)
    extends Tree[T]:
    override type SelfType[-T >: Untyped] = Try[T]

  case class Quote[-T >: Untyped](expr: Tree[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Quote[T]
    def quasi: Boolean = hasAttachment(QuasiSplicing)

  case class UnQuote[-T >: Untyped](expr: Tree[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = UnQuote[T]
    def splicing: Boolean = hasAttachment(QuasiSplicing)

  case class Literal[-T >: Untyped](const: Constant)(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Literal[T]

  /**
   * (define a 1)
   * (define (a: Int) 1)
   * @param sym simple symbol or typed tree
   * @param value
   * @tparam T The type of tree
   */
  case class Define[-T >: Untyped](sym: Tree[T], value: Tree[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Define[T]

  case class LambdaExpression[-T >: Untyped](definition: CaseDef[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = LambdaExpression[T]
    def pattern: Tree[T] = definition.pattern
    def guard: Tree[T] = definition.guard
    def body: Tree[T] = definition.pattern
    def isMacro: Boolean = hasAttachment(Macro)

  case class RecordLiteral[-T >: Untyped](values: List[Tree[T]])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = RecordLiteral[T]

  case class Apply[-T >: Untyped](proc: Tree[T], args: List[Tree[T]])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Apply[T]

  case class Spread[-T >: Untyped](tree: Tree[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Spread[T]

  case class Select[-T >: Untyped](qualifier: Tree[T], name: Symbol[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Select[T]

  case class Typed[-T >: Untyped](expr: Tree[T], tpt: Tree[T])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Typed[T]

  case class Thicket[-T >: Untyped](trees: List[Tree[T]])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = Thicket[T]

    def mapElements(op: Tree[T] => Tree[T] @uncheckedVariance): Thicket[T] =
      val newTrees = trees.mapConserve(op)
      if trees eq newTrees then this
      else Thicket(newTrees)
    end mapElements

    override def hasNoChild: Boolean = trees.isEmpty

    override def span: Spans.Span =
      trees.foldLeft(Spans.NoSpan) { case (spn, tree) =>
        spn.union(tree.span)
      }

    override def withSpan(span: Spans.Span): this.type =
      mapElements(_ withSpan span).asInstanceOf[this.type]

    override def toTreeList: List[Tree[T]] = flatten(trees)

    override def foreachInThicket(op: Tree[T] => Unit): Unit =
      trees foreach (_ foreachInThicket op)

  end Thicket
  
  case class PackageDef[-T >: Untyped](pid: Symbol[T], stats: List[Tree[T]])(using SourceFile) extends Tree[T]:
    override type SelfType[-T >: Untyped] = PackageDef[T]

  def flatten[T >: Untyped](trees: List[Tree[T]]): List[Tree[T]] =
    def recur(buf: ListBuffer[Tree[T]], remaining: List[Tree[T]]): ListBuffer[Tree[T]] =
      remaining match
        case Thicket(elems) :: remain =>
          val buffer =
            if buf == null then
              val b = new ListBuffer[Tree[T]]
              var scanning = trees
              while scanning ne remaining do
                b += scanning.head
                scanning = scanning.tail
              b
            else buf

          recur(recur(buffer, elems), remain)
        case tree :: remain =>
          if buf != null then buf += tree
          recur(buf, remain)
        case _ => buf
    end recur

    val buf = recur(null, trees)
    if buf != null then buf.toList else trees
  end flatten
}

trait TreeInstance[T >: Untyped]:
  type Tree = Tree.Tree[T]
  type Symbol = Tree.Symbol[T]
  type If = Tree.If[T]
  type CaseDef = Tree.CaseDef[T]
  type Match = Tree.Match[T]
  type Block = Tree.Block[T]
  type LisaList = Tree.LisaList[T]
  type Try = Tree.Try[T]
  type Quote = Tree.Quote[T]
  type UnQuote = Tree.UnQuote[T]
  type Literal = Tree.Literal[T]
  type Define = Tree.Define[T]
  type LambdaExpression = Tree.LambdaExpression[T]
  type RecordLiteral = Tree.RecordLiteral[T]
  type Apply = Tree.Apply[T]
  type Spread = Tree.Spread[T]
  type Select = Tree.Select[T]
  type Typed = Tree.Typed[T]
  type Thicket = Tree.Thicket[T]
  type PackageDef = Tree.PackageDef[T]

  import Tree._