package moe.roselia.lisa

object SimpleLispTree {
  sealed trait SimpleLispTree {
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

  case class Value(get: String) extends SimpleLispTree {
    override def toString: String = get

    override def collectVariables: Set[String] = Set(get)
  }
  case class StringLiteral(content: String) extends SimpleLispTree {
    override def toString: String = s"${content.replace("\"", "\\\"")}"
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
}
