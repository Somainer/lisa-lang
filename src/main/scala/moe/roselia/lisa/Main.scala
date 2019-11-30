package moe.roselia.lisa

import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import moe.roselia.lisa.Environments.{CombineEnv, NameSpacedEnv}
import moe.roselia.lisa.Evaluator.EvalResult
import moe.roselia.lisa.LispExp.{NilObj, SString, SideEffectFunction, WrappedScalaObject}

import scala.util.Try
import scala.util.control.NonFatal

object Main {
  import SimpleLispTree._
  import SExpressionParser._
  def printlnErr[S](s: S): Unit = Console.err.println(s)

  @`inline` private def indentLevel(input: String) = input.foldLeft(0)((pairs, c) => {
    if (pairs < 0) pairs
    else if (c == '(') pairs + 1
    else if (c == ')') pairs - 1
    else pairs
  })
  @`inline` private def needMoreInput(input: String) = {
    indentLevel(input) > 0
  }

  private lazy val robot = new java.awt.Robot()
  private def sendKey(code: Int): Unit = {
    robot.keyPress(code)
    robot.keyRelease(code)
  }
  lazy val isMac = {
    System.getProperty("os.name").startsWith("Mac OS")
  }

  private def sendSpace(spaceNum: Int): Unit =
    1 to spaceNum foreach (_ => sendKey(java.awt.event.KeyEvent.VK_SPACE))

  @annotation.tailrec def prompt(env: Environments.Environment, lastInput: String = "", resultIndex: Int = 0): Unit = {
    val lastIndentLevel = indentLevel(lastInput)
    val tabs = if (isMac && lastIndentLevel > 0) " ".repeat(lastIndentLevel << 2) else ""
    if (!isMac && lastIndentLevel > 0) 1 to lastIndentLevel foreach (_ => sendSpace(4))
    val s = scala.io.StdIn
      .readLine(if(lastInput.isEmpty) "lisa>" else s"....>${tabs}")
    val concatInput = s"${lastInput}\n$s".trim
    if (concatInput.nonEmpty) {
      //      println(s"Input: $s")
      if (!concatInput.replace(" ", "").endsWith("\n\n") && needMoreInput(concatInput))
        prompt(env, concatInput, resultIndex)
      else {
        val exp = parseAll(sExpression, concatInput)
        exp match {
          case Success(expression, _) => expression match {
            case SList(List(Value("quit" | "exit"))) =>
              println("Good bye")
            case _ =>
              Try(Evaluator.eval(Evaluator.compile(expression), env))
                .recover{
                  case NonFatal(ex) => Evaluator.EvalFailure(ex.toString)
                }.get match {
                case Evaluator.EvalSuccess(result, newEnv) =>
                  result match {
                    case LispExp.Failure(typ, msg) =>
                      printlnErr(s"$typ: $msg")
                    case NilObj =>
                    case s =>
                      println(s"[$resultIndex]: ${s.tpe.name} = $s")
                  }
                  result match {
                    case NilObj | LispExp.Failure(_, _) =>
                      prompt(newEnv, resultIndex = resultIndex)
                    case s =>
                      prompt(newEnv.withValue(s"[$resultIndex]", s), resultIndex = resultIndex + 1)
                  }
                case f =>
                  printlnErr(s"Runtime Error: $f")
                  prompt(env, resultIndex = resultIndex)
              }
          }
          case f =>
            printlnErr("Parse Error")
            printlnErr(f)
            prompt(env, resultIndex = resultIndex)
        }
      }

    } else prompt(env, resultIndex = resultIndex)
  }

  @throws[java.io.FileNotFoundException]()
  def executeFile(fileName: String, env: Environments.Environment): Environments.Environment = {
    @scala.annotation.tailrec
    def doSeq(source: scala.util.parsing.input.Reader[Char],
              innerEnv: Environments.Environment): Environments.Environment = {
      if (!source.atEnd)
        parse(sExpression, source) match {
          case Success(sExpr, next) =>
            Evaluator.eval(Evaluator.compile(sExpr), innerEnv) match {
              case Evaluator.EvalSuccess(_, nenv) => doSeq(next, nenv)
              case other =>
                printlnErr(other)
                printlnErr(s"\tsource: $sExpr")
                innerEnv
            }
          case Failure(msg, next) =>
            if(!next.atEnd) printlnErr(s"Error: $msg")
            innerEnv
          case Error(msg, _) =>
            printlnErr(s"Fatal: $msg")
            innerEnv
        } else innerEnv
    }
    scala.util.Using(scala.io.Source.fromFile(fileName)) {source => {
      val reader = parse(success(NilObj), source.reader()).next
      doSeq(reader, env.newFrame).newFrame
    }}.get
  }

  @throws[java.io.FileNotFoundException]("on wrong path")
  def compileFile(fromFile: String, toFile: String): Unit = {
    val fromSource = scala.io.Source.fromFile(fromFile)
    val reader = parse(success(NilObj), fromSource.reader()).next
    @annotation.tailrec
    def compileChain(source: scala.util.parsing.input.Reader[Char],
                     acc: List[LispExp.Expression] = Nil): List[LispExp.Expression] = {
      if (!source.atEnd)
        parse(sExpression, source) match {
          case Success(sExpr, next) =>
            Evaluator.compile(sExpr) match {
              case LispExp.Failure(tp, message) =>
                printlnErr(s"Compile Error: $tp", message)
                Nil
              case exp => compileChain(next, exp::acc)
            }

          case Failure(msg, _) =>
            printlnErr(s"Fatal: $msg")
            Nil
        }
      else acc.reverse
    }
    val compiled = compileChain(reader)
    fromSource.close()
    if(compiled.nonEmpty) {
      val file = new FileOutputStream(toFile)
      val oos = new ObjectOutputStream(file)
      oos.writeObject(LispExp.Apply(LispExp.Symbol("group!"), compiled))
      oos.close()
      file.close()
    }
  }

  @throws[java.io.FileNotFoundException]("When wrong from file")
  @throws[java.io.IOException]()
  @throws[ClassCastException]()
  @throws[java.io.StreamCorruptedException]()
  def executeCompiled(fromFile: String, env: Environments.Environment): EvalResult = {
    val fis = new FileInputStream(fromFile)
    val ois = new ObjectInputStream(fis)
    val expr = ois.readObject().asInstanceOf[LispExp.Expression]
    fis.close()
    Evaluator.eval(expr, env)
  }

  def dumpEnv(env: Environments.Environment): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(env)
    oos.close()
    bos.toByteArray
  }

  def main(args: Array[String]): Unit = {
    val preludeEnv =
      CombineEnv(
        Seq(
          Reflect.DotAccessor.accessEnv,
          Preludes.preludeEnvironment,
          NameSpacedEnv("box", Reflect.ToolboxDotAccessor.accessEnv, "")))
        .withValue("load!", SideEffectFunction {
          case (SString(f)::Nil, env) =>
            (NilObj, executeFile(f, env.withValue("__PATH__", SString(f))))
          case (els, env) =>
            (LispExp.Failure("Load error", s"Can only load 1 file but $els found."), env)
        }).withIdentify("prelude").newFrame
    if(args.isEmpty) {
      println(
        """
          |.____    .__
          ||    |   |__| ___________
          ||    |   |  |/  ___/\__  \
          ||    |___|  |\___ \  / __ \_
          ||_______ \__/____  >(____  /
          |        \/       \/      \/
          |Welcome to Lisa REPL by Somainer (moe.roselia.lisa).
          |Type in expressions for evaluation. (quit) to quit.
        """.stripMargin)
      try {
        prompt(preludeEnv)
      } catch {
        case ex: java.lang.StackOverflowError =>
          ex.printStackTrace()
          printlnErr("You input has caused a death loop. Recover you from prelude.")
          main(args)
      }

    }
    else {
      args match {
        case Array(fileName) =>
          executeFile(fileName, preludeEnv.withValue("__PATH__", SString(fileName)))
        case Array("repl", fileName) =>
          prompt(executeFile(
            fileName, preludeEnv.withValue("__PATH__", SString(fileName))
          ))
        case Array("compile", fileName, "-o", toFile) =>
          compileFile(fileName, toFile)
        case Array("execute", fileName) =>
          val result = executeCompiled(fileName, preludeEnv)
          if(!result.isSuccess) printlnErr(s"Error: $result")
        case _ => printlnErr("I could not understand your arguments.")
      }

    }
  }
}
