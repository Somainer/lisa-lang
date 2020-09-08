package moe.roselia.lisa

import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import moe.roselia.lisa.Environments.{CombineEnv, NameSpacedEnv}
import moe.roselia.lisa.Evaluator.EvalResult
import moe.roselia.lisa.Exceptions.{LisaException, LisaRuntimeException, LisaSyntaxException}
import moe.roselia.lisa.Import.PackageImporter
import moe.roselia.lisa.LispExp.{LisaList, NilObj, PrimitiveFunction, SNumber, SString, SideEffectFunction, WrappedScalaObject}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.parsing.input.CharSequenceReader

object Main {
  import SimpleLispTree._
  import SExpressionParser._
  import Util.ConsoleColor.Implicits._
  def printlnErr[S](s: S): Unit = Console.err.println(s.toString.foreground("#ff4a4a"))

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
  def isMac: Boolean = util.Properties.isMac
  lazy val isHeadless: Boolean = {
    java.awt.GraphicsEnvironment.isHeadless
  }
  lazy val shouldFixIndent: Boolean = {
    isHeadless || isMac
  }

  private def sendSpace(spaceNum: Int): Unit =
    1 to spaceNum foreach (_ => sendKey(java.awt.event.KeyEvent.VK_SPACE))

  @annotation.tailrec def prompt(env: Environments.Environment, lastInput: String = "", resultIndex: Int = 0): Unit = {
    val lastIndentLevel = indentLevel(lastInput)
    val tabs = if (shouldFixIndent && lastIndentLevel > 0) " ".repeat(lastIndentLevel << 2) else ""
    if (!shouldFixIndent && lastIndentLevel > 0) 1 to lastIndentLevel foreach (_ => sendSpace(4))
    val inputPrompt = if(lastInput.isEmpty) "lisa>" else s"....>${tabs}"
    val s = scala.io.StdIn
      .readLine(inputPrompt.foreground("#6670ED"))
    val concatInput = s"${lastInput}\n$s".trim
    if (concatInput.nonEmpty) {
      //      println(s"Input: $s")
      if (!concatInput.replace(" ", "").endsWith("\n\n") && needMoreInput(concatInput))
        prompt(env, concatInput, resultIndex)
      else {
        val exp = parseAll(sExpressionOrNil, concatInput)
        exp match {
          case Success(expression, _) => expression match {
            // case SList(List(Value("quit" | "exit"))) =>
            //   println("Good bye")
            case _ =>
              Try(Evaluator.eval(Evaluator.compile(expression), env))
                .recover{
                  case NonFatal(ex) => Evaluator.EvalFailure(ex.toString)
                }.get match {
                case Evaluator.EvalSuccess(result, newEnv) =>
                  result match {
                    case LispExp.Failure(typ, msg) =>
                      printlnErr(s"$typ: $msg")
                    case s if s eq NilObj =>
                    case s =>
                      val indexString = s"[$resultIndex]"
                      println(s"${indexString.foreground("#00AABC")}: " +
                        s"${s.tpe.name.bold.foreground("#890F87")} = ${coloringExpression(s)}")
                  }
                  result match {
                    case LispExp.Failure(_, _) =>
                      prompt(newEnv, resultIndex = resultIndex)
                    case x if x eq NilObj =>
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

  def coloringExpression(exp: LispExp.Expression): String = {
    val literals = "#DD2200"
    val symbols = "#DD0087"
    exp match {
      case _: LispExp.SNumber[_] => exp.toString.foreground(literals)
      case _: LispExp.SBool => exp.toString.foreground(literals)
      case _: LispExp.SAtom => exp.toString.foreground(literals)
      case _: LispExp.Symbol => exp.toString.foreground(symbols)
      case LispExp.JVMNull => exp.toString.foreground(literals)
      case LispExp.Quote(expr) => coloringExpression(expr)
      case LispExp.LisaList(ll) => ll.map(coloringExpression).mkString("(", " ", ")")
      case _ => exp.toString
    }
  }

  /**
   * Execute all lisa codes in a file.
   * @param fileName The name of the file.
   * @param env The starting environment.
   * @throws java.io.FileNotFoundException when the file is not found.
   * @return a tuple, the first element is the last [[Environments.Environment]] executed.
   *         the second is an [[Option]], it will be [[None]] if not error is found, will be [[Some]] containing an
   *         [[LisaException]] if some error is found.
   */
  @throws[java.io.FileNotFoundException]
  def executeFileImpl(fileName: String, env: Environments.Environment): (Environments.Environment, Option[LisaException]) = {
    @scala.annotation.tailrec
    def doSeq(source: scala.util.parsing.input.Reader[Char],
              innerEnv: Environments.Environment): (Environments.Environment, Option[LisaException]) = {
      if (!source.atEnd)
        parse(sExpression, source) match {
          case Success(sExpr, next) =>
            val expr = Evaluator.compile(sExpr)
            Evaluator.eval(expr, innerEnv) match {
              case Evaluator.EvalSuccess(_, nenv) => doSeq(next, nenv)
              case other =>
                // printlnErr(other)
                // printlnErr(s"\tsource: $sExpr")
                (innerEnv, Some(LisaRuntimeException(expr, new RuntimeException(other.toString))))
            }
          case NoSuccess(msg, next) =>
            // if(!next.atEnd) printlnErr(s"Error: $msg")
            if (next.atEnd) (innerEnv, None)
            else (innerEnv, Some(LisaSyntaxException.LisaSyntaxExceptionInFile(msg, fileName, next)))
        } else (innerEnv, None)
    }
    scala.util.Using(scala.io.Source.fromFile(fileName)) { source => {
      val fileContent = source.mkString
      val fileReader = new CharSequenceReader(fileContent match {
        case s if s.startsWith("#!") => s.dropWhile(_ != '\n')
        case s => s
      })
      val reader = parse(success(NilObj), fileReader).next
      doSeq(reader, env.newFrame)
    }}.get
  }

  @throws[java.io.FileNotFoundException]()
  def executeFile(fileName: String, env: Environments.Environment): Environments.Environment = {
    val (environment, errors) = executeFileImpl(fileName, env)
    errors match {
      case Some(LisaSyntaxException(message, source)) =>
        printlnErr(s"syntax error: $message at $source($fileName)")
      case Some(LisaRuntimeException(source, exception)) =>
        printlnErr(s"$exception\n\tsource: $source")
      case _ =>
    }
    environment
  }

  @throws[java.io.FileNotFoundException]
  def executeFileRegardingPath(fileName: String, env: Environments.Environment): Environments.Environment = {
    PackageImporter.withCurrentFilePath(fileName) {
      executeFile(fileName, PackageImporter.environmentWithMeta(fileName, env))
    }
  }

  @throws[java.io.FileNotFoundException]("on wrong path")
  def compileFile(fromFile: String, toFile: String): Unit = {
    val fromSource = scala.io.Source.fromFile(fromFile)
    val reader = parse(success(NilObj), fromSource.reader()).next
    @annotation.tailrec
    def compileChain(source: scala.util.parsing.input.Reader[Char],
                     acc: List[LispExp.Expression] = Nil): List[LispExp.Expression] = {
      if (!source.atEnd) {
        parse(sExpression, source) match {
          case Success(sExpr, next) =>
            Evaluator.compile(sExpr) match {
              case LispExp.Failure(tp, message) =>
                printlnErr(s"Compile Error: $tp", message)
                Nil
              case exp => compileChain(next, exp::acc)
            }

          case NoSuccess(msg, _) =>
            printlnErr(s"Fatal: $msg")
            Nil
        }
      } else acc.reverse
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
    val quitFn = PrimitiveFunction {
      case Nil => sys.exit()
      case LispExp.SInteger(n) :: Nil => sys.exit(n.toInt)
      case _ => throw new IllegalArgumentException(s"An integer expected.")
    }
    val preludeEnv =
      CombineEnv(
        Seq(
          LispExp.LisaRecord.RecordHelperEnv,
          Reflect.DotAccessor.accessEnv,
          Reflect.StaticFieldAccessor.StaticFieldsAccessorEnvironment,
          Preludes.preludeEnvironment,
          NameSpacedEnv("box", Reflect.ToolboxDotAccessor.accessEnv, "")))
        .withValue("load!", SideEffectFunction {
          case (SString(f)::Nil, env) =>
            (NilObj, executeFileRegardingPath(f, env.withValue("__PATH__", SString(f))))
          case (els, env) =>
            (LispExp.Failure("Load error", s"Can only load 1 file but $els found."), env)
        }).withValue("quit", quitFn).withValue("exit", quitFn).withIdentify("prelude").newFrame
    PackageImporter.injectRuntime(preludeEnv)
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
        case Util.ReturnControlFlow.ReturnException(v, _) =>
          printlnErr(s"Caught an unexpected return: $v")
          main(args)
      }

    }
    else {
      args.toList match {
        case "repl" :: fileName :: Nil =>
          prompt(executeFileRegardingPath(
            fileName, preludeEnv.withValue("__PATH__", SString(fileName))
          ))
        case "compile" :: fileName :: "-o" :: toFile :: Nil =>
          compileFile(fileName, toFile)
        case "execute" :: fileName :: Nil =>
          val result = executeCompiled(fileName, preludeEnv)
          if(!result.isSuccess) printlnErr(s"Error: $result")
        case fileName :: arguments =>
          executeFileRegardingPath(fileName,
            preludeEnv.withValue("__PATH__", SString(fileName)).withValue("system/arguments", LisaList(arguments.map(SString))))
        case _ => printlnErr("I could not understand your arguments.")
      }

    }
  }
}
