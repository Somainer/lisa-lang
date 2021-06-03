package moe.lisa.parsing

import moe.lisa.core.SourceFile.SourceFile

import scala.util.parsing.combinator.RegexParsers
import moe.lisa.core.expression.UntypedTree._

import scala.util.matching.Regex

trait NumberParser(using SourceFile) extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  def consumed(p: Parser[Any]): Parser[String] = Parser { in =>
    p(in) match {
      case Success(_, next) =>
        Success(in.source.subSequence(in.offset, next.offset).toString, next)
      case ns: NoSuccess => ns
    }
  }

  def hexDigit: Parser[String] = "[0-9a-fA-F]".r
  def digit: Parser[String] = "[0-9]".r
  def nonZeroDight: Parser[String] = "[1-9]".r

  def floatType: Parser[String] = "F" | "f" | "D" | "d" | "M" | "m"
  def intType: Parser[String] = "L" | "l" | "N" | "n"

  def decimalNumeral = "0" | consumed(nonZeroDight ~ rep(digit))
  def hexNumeral = "0" ~> ("x" | "X") ~> consumed(rep(hexDigit))

  def nonNegativeIntegerLiteral: Parser[Number] = (
    consumed(decimalNumeral <~ opt(intType)).map(Number.infer(_, false)) |||
      consumed(hexNumeral <~ opt(intType)).map(Number.infer(_, false, 16))
    )

  def maybeNegativeNumber(num: Parser[Number]): Parser[Number] =
    ("-" ~> num).map {
      case Number(digits, kind) => Number(s"-$digits", kind)
    } | num
  def integerLiteral = maybeNegativeNumber(nonNegativeIntegerLiteral)

  def exponentPart = consumed(
    ("e" | "E") ~> opt("+" | "-") ~> rep(digit)
  )

  def nonNegativeFloatingPointLiteral = consumed(
    (opt(decimalNumeral) ~> "." ~> rep(digit) ~> opt(exponentPart) ~> opt(floatType)) |||
      (decimalNumeral ~> exponentPart ~> opt(floatType)) |||
      (decimalNumeral ~ floatType)
  ).map(Number.infer(_, true))

  def floatingPointLiteral = maybeNegativeNumber(nonNegativeFloatingPointLiteral)

  def numberLiteral = floatingPointLiteral ||| integerLiteral
}
