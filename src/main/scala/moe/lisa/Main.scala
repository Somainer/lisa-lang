package moe.lisa

import scala.util.chaining._
import moe.lisa.core.SourceFile
import moe.lisa.core.expression.{ShowExpr, ToLisaList, Tree}
import moe.lisa.parsing.LisaParser

import scala.io.StdIn

import moe.lisa.util.macros.ShowScalaAst._

object Main:
  def main(args: Array[String]): Unit =
    moe.lisa.util.macros.ShowScalaAst.inspect {
      def a = 1
      a + 2
      a
    }
    while true do
      print("lisa>")
      val input = StdIn.readLine()
      given SourceFile.SourceFile = SourceFile.SourceFile.virtual("console", input, false)
      val parser: LisaParser = LisaParser.ofSource
      parser.parseAll(parser.sExpression, input) match
        case parser.Success(r, _) =>
          import moe.lisa.compile.transform._
          var tr = r
            .pipe(SpreadOp.makeSpreads)
            .pipe(LambdaLiteralTransform.lambdaLiteral)
            .pipe(NumberToConstant.numberToConstant).inspect
          println(tr)
          val ll = ToLisaList.toLisaList(tr.asInstanceOf[ToLisaList.Tree])
          println(ShowExpr.showIndented(ll)()())

        case f => println(f)
