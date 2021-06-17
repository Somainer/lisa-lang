package moe.lisa.repl

import dotty.tools.dotc.report
import dotty.tools.dotc.util.{SourcePosition, Spans}
import dotty.tools.dotc.core.StdNames.str

import moe.lisa.core.SourceFile
import moe.lisa.core.exceptions.MessageLisaSyntaxError
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Constant
import moe.lisa.parsing.LisaParser
import moe.lisa.util.ThrowHelper


enum ReplCommand:
  case LisaTree(tree: PackageDef)
  case ExecutionTime(tree: Tree)
  case ResetToInitial, Quit, Help, Relax

object ReplCommand:
  def parseInput(input: String): ReplCommand =
    val source = SourceFile.SourceFile.virtual("REPL", input, true)
    parseInput(input, source)

  def parseInput(input: String, source: SourceFile.SourceFile): ReplCommand =
    val parser = LisaParser.ofSource(using source)
    import parser._
    val commands =
      "%%" ~> (
        "quit" ^^^ ReplCommand.Quit
          | "reset" ^^^ ReplCommand.ResetToInitial
          | "help" ^^^ ReplCommand.Help
          | ("time" ~> sExpression ^^ (ReplCommand.ExecutionTime(_)))
        )
    val userInput =
      commands | (compilationUnit.map(ReplCommand.LisaTree(_))) | (success(ReplCommand.LisaTree(PackageDef(Nil)(using source))))
    parseAll(userInput, input) match
      case parser.Success(res, _) =>
        res
      case parser.NoSuccess(msg, loc) =>
        val pos = loc.pos
        val span = Spans.Span(loc.offset)
        throw MessageLisaSyntaxError(msg, Literal(Constant(null))(using source).withSpan(span))
      case _ => ThrowHelper.unreachable()
  end parseInput

  def parseReplInput(input: String)(using state: ReplState) =
    val name = s"${str.REPL_SESSION_LINE}${state.state.objectIndex + 1}"
    val source = SourceFile.SourceFile.virtual(name, input, maybeIncomplete = true)
    parseInput(input, source)
    
  def sourceFileName(i: Int) =
    s"${str.REPL_SESSION_LINE}$i"