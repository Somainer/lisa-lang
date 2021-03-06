package moe.roselia.lisa

import scala.util.parsing.input.OffsetPosition

object SimpleLispTree {
  trait SourceFile {
    def fileName: String
  }
  case class AbstractSourceFile(fileName: String) extends SourceFile
  case class PathSourceFile(path: java.nio.file.Path) extends SourceFile {
    override def fileName: String = path.getFileName.toString
  }

  case class Location(source: CharSequence, startOffset: Int, endOffset: Int) {
    def content: CharSequence = source.subSequence(startOffset, endOffset)

    private lazy val location = OffsetPosition(source, startOffset)
    def line: Int = location.line
    def column: Int = location.column
    def lineContents: String = location.lineContents

    var sourceFile: SourceFile = AbstractSourceFile("")
    def setSourceFile(sourceFile: SourceFile): this.type = {
      this.sourceFile = sourceFile
      this
    }
  }
  val EmptyLocation: Location = Location("", 0, 0)

  trait Locational {
    var location: Location = EmptyLocation
    def setLocation(loc: Location): this.type = {
      if (location eq EmptyLocation) location = loc
      this
    }
  }

  sealed trait SimpleLispTree extends Locational {
    def repr: String = this match {
      case Value(get) => s"Value($get)"
      case SList(children) =>
        if(children.nonEmpty) s"SList(${children.map(_.repr).mkString(", ")})"
        else s"SList( )"
      case StringLiteral(value) => s"String($value)"
      case SQuote(value, false) => s"Quote(${value.repr})"
      case SQuote(value, true) => s"QuasiQuote(${value.repr})"
      case SUnQuote(quoted, false) => s"UnQuote(${quoted.repr})"
      case SUnQuote(quoted, true) => s"UnQuoteSplicing(${quoted.repr})"
    }

    def collectVariables: Set[String] = Set.empty
  }

  trait Value extends SimpleLispTree {
    def get: String
    override def toString: String = get

    override def collectVariables: Set[String] = Set(get)
  }

  object Value {
    def apply(value: String): Value = PlainValue(value)

    def unapply(arg: Value): Option[String] = Some(arg.get)
  }

  case class PlainValue(get: String) extends Value

  case class GraveAccentAtom(get: String) extends Value

  case class StringLiteral(content: String) extends SimpleLispTree {
    override def toString: String = s"${content.replace("\"", "\\\"")}"
  }
  case class StringTemplate(templateName: Value, parts: List[StringLiteral], arguments: List[SimpleLispTree]) extends SimpleLispTree {
     override def toString: String = s"$templateName${StringContext(parts.map(_.content): _*).s(arguments: _*)}"

    override def collectVariables: Set[String] = SList(arguments).collectVariables
  }
  case class SList(list: Seq[SimpleLispTree]) extends SimpleLispTree {
    override def toString: String =
      if(list.isEmpty) "( )"
      else s"( ${list.map(_.toString).mkString(" ")} )"

    override def collectVariables: Set[String] = list.flatMap(_.collectVariables).toSet
  }

  case class SQuote(quote: SimpleLispTree, isQuasiQuote: Boolean) extends SimpleLispTree {
    override def toString: String = if (isQuasiQuote) s"`'$quote" else  s"'$quote"

    override def collectVariables: Set[String] =
      if (isQuasiQuote) quote.collectVariables
      else Set.empty
  }

  case class SUnQuote(quoted: SimpleLispTree, splicing: Boolean) extends SimpleLispTree {
    override def toString: String =
      if (splicing) s"~...$quoted" else s"~$quoted"

    override def collectVariables: Set[String] = quoted.collectVariables
  }

  case class PrecompiledSExpression(exp: LispExp.Expression) extends SimpleLispTree {
    override def toString: String = exp.code
  }

  case class SAtomLeaf(value: String) extends SimpleLispTree {
    override def toString: String = s":$value"
  }
}
