package moe.roselia.lisa

object SimpleLispTree {
  case class Location(source: CharSequence, startOffset: Int, endOffset: Int) {
    def content: CharSequence = source.subSequence(startOffset, endOffset)
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
      case SQuote(value) => s"Quote(${value.repr})"
      case SUnQuote(quoted) => s"UnQuote(${quoted.repr})"
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
  case class StringTemplate(templateName: String, parts: List[String], arguments: List[SimpleLispTree]) extends SimpleLispTree {
     override def toString: String = s"$templateName${StringContext(parts: _*).s(arguments: _*)}"
  }
  case class SList(list: Seq[SimpleLispTree]) extends SimpleLispTree {
    override def toString: String =
      if(list.isEmpty) "( )"
      else s"( ${list.map(_.toString).mkString(" ")} )"

    override def collectVariables: Set[String] = list.flatMap(_.collectVariables).toSet
  }
  case class SQuote(quote: SimpleLispTree) extends SimpleLispTree {
    override def toString: String = s"'$quote"
  }
  case class SUnQuote(quoted: SimpleLispTree) extends SimpleLispTree {
    override def toString: String = s"~$quoted"
  }

  case class PrecompiledSExpression(exp: LispExp.Expression) extends SimpleLispTree {
    override def toString: String = exp.code
  }

  case class SAtomLeaf(value: String) extends SimpleLispTree {
    override def toString: String = s":$value"
  }
}
