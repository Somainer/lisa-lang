package moe.roselia.lisa.Repl

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{JVMNull, PolymorphicExpression, PrimitiveMacro, Procedure, SBool, SNumber, SideEffectFunction, SimpleMacroClosure, Symbol}
import moe.roselia.lisa.SimpleLispTree
import moe.roselia.lisa.SimpleLispTree.SimpleLispTree
import moe.roselia.lisa.Util.ConsoleColor.RGBColor

trait ASTHighlighter {
  val variableColor: RGBColor = RGBColor.parseHexString("#66ccff")
  def highlightTree(tree: SimpleLispTree, source: String)(environment: Environment): String =
    highlightTree(tree :: Nil, source)(environment)

  def highlightTree(tree: Seq[SimpleLispTree], source: String)(environment: Environment = EmptyEnv): String = {
    import moe.roselia.lisa.Util.ConsoleColor.Implicits._

    val flattenEnv = environment.flatten // To avoid accidentally reflection calls.
    val sourceBuffer = source.map(_.toString).map(new StringBuilder(_)).toArray
    def replaceRange(from: Int, to: Int, content: String): Unit = {
      sourceBuffer(from).clear()
      sourceBuffer(from).addAll(content)
      val clearRange = from until to
      if (clearRange.nonEmpty) clearRange.tail.foreach(sourceBuffer(_).clear())
    }

    def transform(tree: SimpleLispTree)(transformer: String => String): Unit =
      replaceRange(tree.location.startOffset, tree.location.endOffset, transformer(tree.location.content.toString))

    def traverse(tree: SimpleLispTree): Unit = tree match {
      case value: SimpleLispTree.Value =>
        Evaluator.compileToList(value) match {
          case _: SNumber[_] | _: SBool | JVMNull => transform(tree)(_.ansiRed) // Literal
          case Symbol(keyword) if LisaTerminal.keywords.contains(keyword) => transform(tree)(_.ansiYellow) // Keyword
          case Symbol(dotAcc) if dotAcc.startsWith(".") =>
            transform(tree)(_.foreground(LisaTerminal.roseliaColor).bold)
          case Symbol(sym) =>
            flattenEnv.getValueOption(sym).foreach {
              case _ if sym.endsWith("!") => transform(tree)(_.ansiMagenta)
              case _ if sym.endsWith("?") => transform(tree)(_.ansiGreen)
              case _: PrimitiveMacro
                   | _: SimpleMacroClosure
                   | PolymorphicExpression(_, _, _, true) => transform(tree)(_.ansiCyan.italic)
              case _: SideEffectFunction => transform(tree)(_.ansiCyan.underline)
              case _: Procedure => transform(tree)(_.ansiCyan)
              case _ =>
                transform(tree) { token =>
                  val colorized = token.foreground(variableColor)
                  if (flattenEnv.isMutable(sym)) colorized.underline
                  else colorized
                }
            }
          case _ =>
        }
      case sl @ SimpleLispTree.StringLiteral(content) =>
        transform(sl)(_.ansiRed)
      case st @ SimpleLispTree.StringTemplate(templateName, parts, arguments) =>
//        arguments.foreach(traverse)
//        if (templateName.get == "$") transform(templateName)(_.ansiCyan.bold) else traverse(templateName)
//        parts.foreach(traverse)
        transform(st)(_.ansiRed)
      case SimpleLispTree.SList(list) =>
        list.foreach(traverse)
      case SimpleLispTree.SQuote(quote, _) =>
        traverse(quote)
      case sq @ SimpleLispTree.SUnQuote(quoted, _) => transform(sq)(_.italic)
      case SimpleLispTree.PrecompiledSExpression(exp) =>
      case value @ SimpleLispTree.SAtomLeaf(_) =>
        transform(value)(_.ansiRed)
    }

    tree foreach traverse
    sourceBuffer.foldLeft(new StringBuilder)(_ addAll _).result()
  }
}
object ASTHighlighter extends ASTHighlighter
