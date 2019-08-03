package moe.roselia.lisa

import moe.roselia.lisa.SimpleLispTree._

import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object SExpressionParser extends ImplicitConversions with RegexParsers {
  def sValue = "[^() \\s]+".r map Value named "Values"

  def string = "\"(((\\\\\")|[^\"])*)\"".r
    .map(_.replace("\\\"", "\""))
    .map(_.drop(1).dropRight(1)) named "String Literals"

  def stringValue = string.map(StringLiteral) named "String Values"

  def sQuote = "'" ~> sExpression map SQuote

  def sExpression: Parser[SimpleLispTree] =
    ("(" ~> rep(sExpression) <~ ")" map SList) | stringValue | sQuote | sValue

  def eof = "\\z".r named "End of line"

  def root[A](p: Parser[A]) = p <~ eof
}
