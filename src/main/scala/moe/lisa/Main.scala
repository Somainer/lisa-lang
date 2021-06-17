package moe.lisa

import moe.lisa.compile.LisaDriver

import scala.util.chaining._
import moe.lisa.core.SourceFile
import moe.lisa.core.exceptions.LisaSyntaxError
import moe.lisa.core.expression.{ShowExpr, ToLisaList, Tree}
import moe.lisa.parsing.LisaParser
import moe.lisa.repl.LisaReplDriver
import moe.lisa.util.PathUtil

import scala.io.StdIn
import moe.lisa.util.macros.ShowScalaAst._

object Main:
  val message =
    """
      |.____    .__
      ||    |   |__| ___________
      ||    |   |  |/  ___/\__  \
      ||    |___|  |\___ \  / __ \_
      ||_______ \__/____  >(____  /
      |        \/       \/      \/
      |Welcome to Lisa REPL (moe.lisa).
      |Type in expressions for evaluation. (quit) to quit.
      """.stripMargin
  def main(args: Array[String]): Unit =
    new LisaDriver().main(enrichArgs(args))

  def enrichArgs(args: Array[String]) =
    var arrayBuf = collection.mutable.ArrayBuffer.from(args)
    if !args.contains("-classpath") then
      arrayBuf += "-classpath"
      arrayBuf += PathUtil.getClassPath
    arrayBuf.toArray

object ReplMain:
  def main(args: Array[String]): Unit =
    if args.isEmpty then println(Main.message)
    new LisaReplDriver(Main.enrichArgs(args)).runLoop()
