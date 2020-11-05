package moe.roselia.lisa.Repl

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{JVMNull, PolymorphicExpression, PrimitiveMacro, Procedure, SBool, SNumber, SimpleMacroClosure, Symbol}
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
      from.until(to).tail.foreach(sourceBuffer(_).clear())
    }

    def transform(tree: SimpleLispTree)(transformer: String => String): Unit =
      replaceRange(tree.location.startOffset, tree.location.endOffset, transformer(tree.location.content.toString))

    def traverse(tree: SimpleLispTree): Unit = tree match {
      case value: SimpleLispTree.Value =>
        Evaluator.compileToList(value) match {
          case _: SNumber[_] | _: SBool | JVMNull => transform(tree)(_.ansiRed)
          case Symbol("define" | "lambda" | "define-macro") => transform(tree)(_.ansiYellow)
          case Symbol(sym) =>
            flattenEnv.getValueOption(sym).foreach {
              case _ if sym.endsWith("!") => transform(tree)(_.ansiMagenta)
              case _ if sym.endsWith("?") => transform(tree)(_.ansiGreen)
              case _: PrimitiveMacro
                   | _: SimpleMacroClosure
                   | PolymorphicExpression(_, _, _, false) => transform(tree)(_.ansiCyan.italic)
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
        arguments.foreach(traverse)
        transform(st)(_.ansiRed)
      case SimpleLispTree.SList(list) =>
        list.foreach(traverse)
      case SimpleLispTree.SQuote(quote) =>
        traverse(quote)
      case sq @ SimpleLispTree.SUnQuote(quoted) => transform(sq)(_.italic)
      case SimpleLispTree.PrecompiledSExpression(exp) =>
      case value @ SimpleLispTree.SAtomLeaf(_) =>
        transform(value)(_.ansiRed)
    }

    tree foreach traverse
    sourceBuffer.foldLeft(new StringBuilder)(_ addAll _).result()
  }
}
object ASTHighlighter extends ASTHighlighter
