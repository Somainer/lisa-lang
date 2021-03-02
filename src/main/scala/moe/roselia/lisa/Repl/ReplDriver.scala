package moe.roselia.lisa.Repl

import java.io.{InputStream, PrintStream}

import scala.jdk.CollectionConverters._
import jline.console.UserInterruptException
import moe.roselia.lisa.Environments.Environment
import moe.roselia.lisa.Repl.Commands.{ExecutionTime, Help, InterpreterCommand, LisaExpressionTree, Quit, ResetToInitial}
import org.jline.reader.{Candidate, EndOfFileException}
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.Reflect.{ConstructorCaller, StaticFieldAccessor}
import moe.roselia.lisa.SExpressionParser
import moe.roselia.lisa.LispExp
import moe.roselia.lisa.LispExp.{Expression, NilObj}
import moe.roselia.lisa.Main.coloringExpression
import moe.roselia.lisa.Util.ConsoleColor.Implicits._

class ReplDriver(inputStream: InputStream = System.in,
                outputStream: PrintStream = System.out,
                errorStream: PrintStream = System.err
                ) {
  var savedInitialState: Option[State] = None
  val initialState: State = State(0, moe.roselia.lisa.Preludes.preludeEnvironment)

  def interpretCommand(commands: InterpreterCommand)(implicit state: State): State = {
    commands match {
      case ResetToInitial =>
        savedInitialState.getOrElse(initialState)
      case Help =>
        println(Help.message)
        state
      case Quit => state
      case ExecutionTime(expressionTree) =>
        val start = System.nanoTime()
        val newState = evaluateExpression(Evaluator.compile(expressionTree))
        val elapsed = System.nanoTime() - start
        elapsed match {
          case x if x < 1000000 => println(s"Wall Time: $x ns.")
          case x if x < 1e9 => println(s"Wall Time: ${x / 1e6} ms.")
          case x => println(s"Wall Time: ${x / 1e9} s.")
        }
        newState
      case LisaExpressionTree(tree) =>
        tree.foldLeft(state)((state, exprTree) => evaluateExpression(Evaluator.compile(exprTree))(state))
      case Commands.OldRepl =>
        moe.roselia.lisa.Main.prompt(
          state.environment,
          "",
          state.resultIndex
        )
        state
    }
  }

  def printlnErr[S](s: S): Unit = errorStream.println(s.toString.foreground("#ff4a4a"))

  def evaluateExpression(expression: => Expression)(implicit state: State): State = {
    def newStateWith(index: Int = state.resultIndex, environment: Environment = state.environment) =
      state.copy(
        resultIndex = index,
        environment = environment
      )
    util.Try { Evaluator.eval(expression, state.environment) }.recover {
      case util.control.NonFatal(ex) => Evaluator.EvalFailure.fromThrowable(ex)
    }.get match {
      case Evaluator.EvalSuccess(result, newEnv) =>
        result match {
          case LispExp.Failure(typ, msg) =>
            printlnErr(s"$typ: $msg")
            newStateWith(environment = newEnv)
          case s if s eq NilObj =>
            newStateWith(environment = newEnv)
          case s =>
            val indexString = s"[${state.resultIndex}]"
            println(s"${indexString.foreground("#00AABC")}: ${s.tpe.name.bold.foreground("#890F87")} = ${coloringExpression(s)}")
            newStateWith(
              index = state.resultIndex + 1,
              environment = newEnv.withValue(indexString, s)
            )
        }
      case f =>
        printlnErr(s"Runtime Error: $f")
        state
    }
  }

  def runUntilQuit(environment: Environment): State = {
    runUntilQuit(State(0, environment))
  }

  def runUntilQuit(initialState: State = this.initialState): State = {
    savedInitialState = Some(initialState)
    val terminal = new LisaTerminal
    def readLine(state: State): String = {
      try {
        val line = terminal.readLine { (self, line, candidates) =>
          val rawWord = line.word()
          val prefix = rawWord.takeWhile(_ == '(')
          val word = rawWord.dropWhile(_ == '(')
          LisaTerminal.provideKeywordHint(word).foreach { name =>
            candidates.add(new Candidate(
              s"$prefix$name", s"$name: Keyword",
              null, null, null, null, true
            ))
          }
          val allDefinedValues = state.environment.collectDefinedValues.toSeq
          (allDefinedValues.filter(_.toLowerCase.startsWith(word.toLowerCase)) ++
            HintProvider.findSimilarSymbols(word, allDefinedValues))
            .foreach { item =>
              val value = state.environment.get(item)
              candidates.add(new Candidate(
                s"$prefix$item",
                s"$item: ${value.tpe.name}",
                null, if (value.docString.isEmpty) null else value.docString, null, null, false
              ))
            }
          val beforeWord = line.line().substring(0, line.cursor()).reverseIterator.takeWhile(_ != '(').mkString.reverse
          HintProvider
            .provideMacroHint(beforeWord, word)(state.environment.flatten)
            .foreach { case (value, display) =>
              candidates.add(new Candidate(
                s"$prefix$value", display,
                null, null, null, null, false
              ))
            }
          if (word.contains("/")) {
            val (className, rawMethodName) = word.splitAt(word.indexOf('/'))
            val methodName = rawMethodName.tail
            try {
              val clazz = ConstructorCaller.resolveClassBySimpleName(className)
              clazz.getDeclaredMethods.filter(StaticFieldAccessor.memberIsStatic).filter(_.getName.startsWith(methodName)).foreach { method =>
                candidates.add(new Candidate(
                  s"$prefix$className/${method.getName}",
                  s"${method.getName}: (${method.getParameterTypes.map(_.getSimpleName).mkString(", ")}) => ${method.getReturnType.getSimpleName} ",
                  null,
                  null,
                  null, null, true
                ))
              }
              clazz.getDeclaredFields.filter(StaticFieldAccessor.memberIsStatic).filter(_.getName.startsWith(methodName)).foreach { field =>
                candidates.add(new Candidate(
                  s"$prefix$className/${field.getName}",
                  s"${field.getName}: ${field.getType.getSimpleName}",
                  null, null, null, null, true
                ))
              }
            } catch {
              case _: ClassNotFoundException => // Just do nothing.
            }
          }

          if (word.startsWith(".")) {
            HintProvider
              .provideMemberSymbols(word.substring(1), line.line().substring(line.cursor()))(state)
              .foreach { member =>
                val memberName = member.name.decodedName.toString
                val typeName = HintProvider.symbolToTypeSignature(member)
                candidates.add(new Candidate(
                  s"$prefix.$memberName",
                  s"$memberName: $typeName",
                  null, null, null, null, false
                ))
              }
          }
        }(state)
        line
      } catch {
        case _: EndOfFileException |
          _: UserInterruptException => "%%quit"
      }
    }

    @annotation.tailrec
    def loop(state: State): State = {
      val line = readLine(state)
      parseInput(line) match {
        case SExpressionParser.Success(command, _) =>
          if (command == Commands.OldRepl) terminal.close()
          if (command == Quit) state
          else loop(interpretCommand(command)(state))
        case f =>
          printlnErr(f)
          loop(state)
      }
    }

    try withRedirectedStreams {
      loop(initialState)
    } finally {
      terminal.close()
    }
  }

  def parseInput(input: String): SExpressionParser.ParseResult[InterpreterCommand] = {
    import moe.roselia.lisa.SExpressionParser._
    val commands: Parser[InterpreterCommand] =
      "%%" ~> (
        "quit" ^^^ Commands.Quit
          | "reset" ^^^ Commands.ResetToInitial
          | "help" ^^^ Commands.Help
          | ("time" ~> sExpression ^^ Commands.ExecutionTime)
          | "old-repl" ^^^ Commands.OldRepl
        )
    val userInput = commands | (rep(sExpression) | success(Nil)) ^^ { Commands.LisaExpressionTree }

    parseAll(userInput, input)
  }

  def withRedirectedStreams[T](op: => T): T = {
    val savedIn = System.in
    val savedOut = System.out
    val savedErr = System.err

    try {
      System.setIn(inputStream)
      System.setOut(outputStream)
      System.setErr(errorStream)
      op
    } finally {
      System.setIn(savedIn)
      System.setOut(savedOut)
      System.setErr(savedErr)
    }
  }
}
