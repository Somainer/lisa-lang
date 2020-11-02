package moe.roselia.lisa.Repl

import moe.roselia.lisa.SimpleLispTree.SimpleLispTree

object Commands {
  trait InterpreterCommand
  
  case class LisaExpressionTree(expression: SimpleLispTree) extends InterpreterCommand
  case class ExecutionTime(expressionTree: SimpleLispTree) extends InterpreterCommand
  case object ResetToInitial extends InterpreterCommand
  case object Quit extends InterpreterCommand
  case object Help extends InterpreterCommand {
    val message: String =
      """The REPL has several commands available:
        |
        |%%help                    print this summary
        |%%quit                    exit the interpreter
        |%%reset                   reset the repl to its initial state, forgetting all session entries
        |%%time <expression>       test the execution time of an expression
      """.stripMargin
  }
}
