package moe.lisa.lang

import moe.lisa.core.SourceFile
import moe.lisa.core.expression.ToLisaList
import moe.lisa.core.expression.Tree

import scala.io.StdIn
import scala.collection.immutable.NumericRange
import moe.lisa.parsing.LisaParser
import moe.lisa.runtime.constructs.LisaObjects

trait Predef {
  type Rational = moe.lisa.lang.Rational[BigInt]
  def ratio(num: BigInt, denom: BigInt = 1): Rational = Rational(num, denom)
  def int(n: String): Int = n.toInt
  def int[T](n: T)(using num: Numeric[T]): Int = num.toInt(n)
  def int(n: Boolean): Int = if n then 1 else 0

  def `truthy?`(n: String): Boolean = !n.isEmpty
  def `truthy?`(n: Seq[Any]): Boolean = !n.isEmpty
  def `truthy?`[T: Numeric](n: T): Boolean = n != 0

  def `string->symbol`(s: String) = Symbol(s)
  def `string->atom`(s: String) = Atom(s)

  def string(parts: Any*): String = parts.mkString

  def `string/interpolate`(parts: Seq[String], arguments: Seq[Any]): String =
    StringContext(parts*).s(arguments)

  def quit() = scala.sys.exit()
  def quit(code: Int) = scala.sys.exit(code)
  def exit() = scala.sys.exit()
  def exit(code: Int) = scala.sys.exit(code)

  def range[T: Integral](from: T, to: T) =
    NumericRange(from, to, summon.one)
  def range[T: Integral](from: T, to: T, step: T) =
    NumericRange(from, to, step)
  def range(from: Int, to: Int) =
    Range(from, to, 1)
  def range(from: Int, to: Int, step: Int) =
    Range(from, to, step)

  def list(xs: Any*): List[Any] = xs.toList

  def `range/inclusive`[T: Integral](from: T, to: T) =
    NumericRange.inclusive(from, to, summon.one)
  def `range/inclusive`[T: Integral](from: T, to: T, step: T) =
    NumericRange.inclusive(from, to, step)

  def `read-string`(s: String): Any =
    val parser = LisaParser.ofSource(using SourceFile.SourceFile.virtual("<read-string>", s))
    parser.parseAll(parser.sExpression, s) match
      case parser.Success(exp, _) =>
        val converter = ToLisaList.of[Tree.Untyped]
        val ls = converter.toLisaList(exp)
        converter.toValueList(ls)
      case f =>
        throw new RuntimeException(f.toString)

  def `read-many-from-string`(s: String): List[Any] =
    val parser = LisaParser.ofSource(using SourceFile.SourceFile.virtual("<read-string>", s))
    import parser._
    parseAll(rep(sExpression) | success(Nil), s) match
      case Success(exp, _) =>
        val converter = ToLisaList.of[Tree.Untyped]
        val ls = exp.map(converter.toLisaList)
        ls.map(converter.toValueList)
      case f =>
        throw new RuntimeException(f.toString)

  def read(): Any =
    `read-string`(StdIn.readLine())
  def `read-many`(): Seq[Any] =
    val input = Iterator.continually(StdIn.readLine).takeWhile(_ ne null)
    `read-many-from-string`(input.mkString)

  def panic(msg: String) = throw new RuntimeException(msg)
  def `panic!`(msg: Any*) = throw new RuntimeException(msg.mkString(" "))
  def `print!`(xs: Any*): Unit =
    print(xs.mkString(" "))
  def `println!`(xs: Any*): Unit =
    println(xs.mkString(" "))
  def input(xs: Any*): String =
    print(xs.mkString(" "))
    StdIn.readLine()
}

object Predef extends Predef

trait DynamicPredef {
}

object DynamicPredef extends DynamicPredef
