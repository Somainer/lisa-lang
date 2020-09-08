package moe.roselia.lisa.Exceptions

trait LisaSyntaxException extends LisaException {
  override def toString: String = this match {
    case LisaSyntaxException(message, source) =>
      s"${getClass.getSimpleName}: $message at $source"
    case _ => super.toString
  }
}
object LisaSyntaxException {
  case class LisaSyntaxExceptionInInput(message: String, source: String) extends Exception(message) with LisaException
  case class LisaSyntaxExceptionInFile(message: String, sourceFile: String, reader: util.parsing.input.Reader[Char])
    extends Exception(message) with LisaSyntaxException

  def unapply(arg: LisaException): Option[(String, String)] = arg match {
    case LisaSyntaxExceptionInInput(message, source) => Some((message, source))
    case LisaSyntaxExceptionInFile(message, sourceFile, reader) =>
      Some((message, if (!reader.atEnd) s"$sourceFile [${reader.pos.line}:${reader.pos.column}]:\n${reader.pos.longString}" else ""))
    case _ => None
  }
}
