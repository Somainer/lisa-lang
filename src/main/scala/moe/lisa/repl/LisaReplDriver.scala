package moe.lisa.repl

import dotty.tools.repl.State
import dotty.tools.repl.ReplDriver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, StoreReporter, ConsoleReporter}
import dotty.tools.io.VirtualDirectory
import dotty.tools.dotc.core.StdNames.str
import moe.lisa.compile.transform.GenAst
import moe.lisa.core.SourceFile.SourceFile

import java.io.PrintStream
import org.jline.reader._
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.UntypedTree.{UntypedTreeCopier => cpy}

case class ReplState(state: State)

class LisaReplDriver(
  settings: Array[String],
  out: PrintStream = Console.out,
  classLoader: Option[ClassLoader] = None
  ) extends ReplDriver(settings, out, classLoader) {

  def initReplState: ReplState = ReplState(initialState)
  def resetToInitialState() = {
    compiler = new LisaReplCompiler
    rootCtx = initReplState.state.context
    if (rootCtx.settings.outputDir.isDefault(using rootCtx))
      rootCtx = rootCtx.fresh
        .setSetting(rootCtx.settings.outputDir, new VirtualDirectory("<REPL compilation output>"))
  }

  var compiler: LisaReplCompiler = _
  var rootCtx: Context = _

  resetToInitialState()

  def runLoop(state: ReplState = initReplState): ReplState =
    val terminal = new LisaTerminal

    def readLine(using ReplState): ReplCommand = {
      val completer: Completer = { (_, line, candidates) =>
        ()
      }

      try
        val line = terminal.readLine(completer)
        compiler.reported(ReplCommand.Relax)(ReplCommand.parseReplInput(line))
      catch
        case _: EndOfFileException |
          _: UserInterruptException => ReplCommand.Quit
    }

    @annotation.tailrec
    def loop(state: ReplState): ReplState = {
      val res = readLine(using state)
      if res == ReplCommand.Quit then state
      else
        val newState =
          try interpretCommand(res)(using state)
          catch
            case ex: AssertionError =>
              out.println("Some critial assetion failed.")
              ex.printStackTrace(out)
              state
        loop(newState)
    }

    try
      withRedirectedOutput { loop(state) }
    finally terminal.close()
  end runLoop

  def interpretCommand(command: ReplCommand)(using state: ReplState): ReplState = {
    import ReplCommand._
    command match
      case Quit => state
      case ResetToInitial => initReplState
      case Help =>
        println("No way")
        state
      case LisaTree(tree) if tree.stats.isEmpty => state
      case LisaTree(tree) =>
        compileAndRun(tree, state)
      case ExecutionTime(e) =>
        // TODO: Finish it
        println("Not supported.")
        state
      case _ => state
  }

  private def newRun(state: State) = {
    val run = compiler.newRun(rootCtx.fresh.setReporter(new StoreReporter()), state)
    state.copy(context = run.runContext)
  }

  def compileAndRun(parsed: PackageDef, state: ReplState): ReplState = {
    val replState = {
      val state0 = newRun(state.state)
      state.copy(state0.copy(context = state0.context.withSource(parsed.source)))
    }
    val (trees, state1) = getDefines(parsed.stats.map(compiler.astGenReported(_)(using replState)), replState)
    compileDefines(trees, state1)
//    compileDefines(parsed.stats, replState)
  }

  def compileDefines(trees: List[Tree], state: ReplState): ReplState = {
    given ReplState = state
    compiler.compileLisa(trees).fold(
      displayErrors,
      (unit, newState) => {
        val printer = new ReplPrinter(classLoader)
        printer
          .printDefinedValues(trees, unit, newState)
          .foreach(d => out.println(d.msg))
        newState
      }
    )
  }

  def getDefines(trees: List[Tree], state: ReplState): (List[Tree], ReplState) = {
    val flattened = trees.flatMap(_.toTreeList)
    var defineIndex = state.state.valIndex
    val defs = collection.mutable.ListBuffer.empty[Tree]

    flattened.foreach {
      case expr @ Define(_, _) => defs += expr
      case expr =>
        val resName = s"${str.REPL_RES_PREFIX}$defineIndex"
        defineIndex += 1
        val define = cpy.Define(expr)(cpy.Symbol(expr)(resName), expr)
        defs += define
    }

    val state1 = state.state.copy(
      // Advanced by ReplCompiler
//      objectIndex = state.state.objectIndex + 1,
      valIndex = defineIndex
    )
    (defs.toList, state.copy(state1))
  }

  private def displayErrors(errs: Seq[Diagnostic])(using state: ReplState): ReplState = {
    given context: Context = state.state.context
//    errs.foreach(context.reporter.report(_)(using context))
    val reporter = new ConsoleReporter()
//    out.println(reporter.summary)
    errs.foreach(reporter.report)
    state
  }

  def withRedirectedOutput[T](op: => T): T = {
    val savedOut = System.out
    val savedErr = System.err
    try
      System.setOut(out)
      System.setErr(out)
      op
    finally
      System.setOut(savedOut)
      System.setErr(savedErr)
  }
}
