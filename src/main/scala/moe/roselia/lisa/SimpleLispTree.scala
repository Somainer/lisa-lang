package moe.roselia.lisa

object SimpleLispTree {
  sealed trait SimpleLispTree {
    def repr: String = this match {
      case Value(get) => s"Value($get)"
      case SList(children) =>
        if(children.nonEmpty) s"SList(${children.map(_.repr).mkString(", ")})"
        else s"SList( )"
      case StringLiteral(value) => s"String($value)"
    }
  }

  case class Value(get: String) extends SimpleLispTree {
    override def toString: String = get
  }
  case class StringLiteral(content: String) extends SimpleLispTree {
    override def toString: String = s"${content.replace("\"", "\\\"")}"
  }
  case class SList(list: Seq[SimpleLispTree]) extends SimpleLispTree {
    override def toString: String =
      if(list.isEmpty) "( )"
      else s"( ${list.map(_.toString).mkString(" ")} )"
  }
}
