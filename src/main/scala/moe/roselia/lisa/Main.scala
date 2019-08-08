package moe.roselia.lisa

import moe.roselia.lisa.Environments.CombineEnv
import moe.roselia.lisa.LispExp.{NilObj, SString, SideEffectFunction}

import scala.util.Try
import scala.util.control.NonFatal

object Main {
  import SimpleLispTree._
  import SExpressionParser._
  @annotation.tailrec def prompt(env: Environments.Environment): Unit = {
    val s = scala.io.StdIn.readLine("lisa>")
    if (s.nonEmpty) {
      //      println(s"Input: $s")
      val exp = parse(root(sExpression), s)
      exp match {
        case Success(expression, _) => expression match {
          case SList(List(Value("quit" | "exit"))) =>
            println("Good bye")
          case _ =>
            Try(Evaluator.eval(Evaluator.compile(expression), env))
              .recover{
                case NonFatal(ex) => Evaluator.EvalFailure(ex.getLocalizedMessage)
              }.get match {
              case Evaluator.EvalSuccess(result, newEnv) =>
                result match {
                  case LispExp.Failure(typ, msg) =>
                    println(s"$typ: $msg")
                  case NilObj =>
                  case s => println(s)
                }
                prompt(newEnv)
              case f =>
                println(s"Runtime Error: $f")
                prompt(env)
            }
        }
        case f =>
          println("Parse Error")
          println(f)
          prompt(env)
      }
    } else prompt(env)
  }
  def executeFile(fileName: String, env: Environments.Environment): Environments.Environment = {
    val reader = parse(success(NilObj), scala.io.Source.fromFile(fileName).reader()).next
    @scala.annotation.tailrec
    def doSeq(source: scala.util.parsing.input.Reader[Char],
              innerEnv: Environments.Environment): Environments.Environment = {
      if (!source.atEnd)
        parse(sExpression, source) match {
          case Success(sExpr, next) =>
            Evaluator.eval(Evaluator.compile(sExpr), innerEnv) match {
              case Evaluator.EvalSuccess(_, nenv) => doSeq(next, nenv)
              case other =>
                println(other)
                innerEnv
            }
          case Failure(msg, next) =>
            if(!next.atEnd) println(s"Error: $msg")
            innerEnv
          case Error(msg, _) =>
            println(s"Fatal: $msg")
            innerEnv
        } else innerEnv
    }
    doSeq(reader, env)
  }
  def main(args: Array[String]): Unit = {
    val preludeEnv =
      CombineEnv(Seq(Reflect.DotAccessor.accessEnv, Preludes.preludeEnvironment))
        .withValue("load!", SideEffectFunction {
          case (SString(f)::Nil, env) =>
            (NilObj, executeFile(f, env.withValue("__PATH__", SString(f))))
          case (els, env) =>
            (LispExp.Failure("Load error", s"Can only load 1 file but $els found."), env)
        })
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
      prompt(preludeEnv)
    }
    else {
      args match {
        case Array(fileName) =>
          executeFile(fileName, preludeEnv.withValue("__PATH__", SString(fileName)))
        case Array("repl", fileName) =>
          prompt(executeFile(
            fileName, preludeEnv.withValue("__PATH__", SString(fileName))
          ))
      }

    }
  }
}
