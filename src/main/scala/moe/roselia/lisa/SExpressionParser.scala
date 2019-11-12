package moe.roselia.lisa

import moe.roselia.lisa.LispExp.{Apply, LambdaExpression, PlainSymbol, Symbol}
import moe.roselia.lisa.SimpleLispTree._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object SExpressionParser extends ImplicitConversions with RegexParsers {
  override protected val whiteSpace = """(\s|;.*)+""".r

  def sValue = sValueExclude("")
  def sValueExclude(ex: String) =
    ("`.+`".r.map(_.drop(1).dropRight(1)).map(GraveAccentAtom) | s"[^() ${Regex.quote(ex)}\\s]+".r.map(PlainValue)) named "Values"

  def string = "\"(((\\\\\")|[^\"])*)\"".r
    .flatMap(x =>
      scala.util
        .Try(StringContext.processEscapes(x))
        .fold(ex => err(ex.getLocalizedMessage), success))
    .map(_.drop(1).dropRight(1)) named "String Literals"

  def stringValue = string.map(StringLiteral) named "String Values"

  def sQuote = "'" ~> sExpression map SQuote

  def sUnquote = "~" ~> sExpression map SUnQuote

  def sExpression: Parser[SimpleLispTree] =
    ("(" ~> rep(sExpression) <~ ")" map SList) | stringValue | lambdaHelper | sQuote | sUnquote | sValue

  def eof = "\\z".r named "End of line"

  def root[A](p: Parser[A]) = p <~ eof

  def integer = "\\d+".r map (_.toInt)

  def lambdaHelper = "&" ~> ((sValueExclude("/") ~ ("/" ~> integer) ^^ {
    case s ~ i =>
      val argsList = (0 until i).toList map (x => s"arg$x") map (PlainSymbol)
      PrecompiledSExpression(LambdaExpression(Apply(Evaluator.compile(s), argsList), argsList))
  }) | sExpression.map(ex => {
    val variables = ex.collectVariables.filter(_.matches("#\\d*")).toIndexedSeq.sortBy {
      case "#" => -1
      case s"#$i" => i.toInt
    }.map(PlainSymbol).toList
    PrecompiledSExpression(LambdaExpression(Evaluator.compile(ex),
      if(variables.isEmpty) Apply(Symbol("..."), Symbol("_")::Nil)::Nil else variables))
  }))
}
