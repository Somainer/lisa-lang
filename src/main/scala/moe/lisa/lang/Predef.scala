package moe.lisa.lang

import scala.io.StdIn

import moe.lisa.parsing.LisaParser
import moe.lisa.runtime.constructs.LisaObjects

object Predef { predef =>
  def int(n: String): Int = n.toInt
  def int(n: Int): Int = n
  def int(n: Long): Int = n.toInt
  def int(n: Boolean): Int = if n then 1 else 0

  def `truthy?`(n: String): Boolean = !n.isEmpty
  def `truthy?`(n: Seq[Any]): Boolean = !n.isEmpty
  def `truthy?`[T: Numeric](n: T): Boolean = n != 0

  def `string->symbol`(s: String) = Symbol(s)
  def `string->atom`(s: String) = Atom(s)

  def `string/interpolate`(parts: Seq[String], arguments: Seq[Any]): String =
    StringContext(parts*).s(arguments)

  def quit() = scala.sys.exit()
  def quit(code: Int) = scala.sys.exit(code)
  def exit() = scala.sys.exit()
  def exit(code: Int) = scala.sys.exit(code)

  def `read-string`(s: String): Any = ???

  def `read-many-from-string`(s: String): Seq[Any] = ???

  def read(): Any =
    `read-string`(StdIn.readLine())
  def `read-many`(): Seq[Any] =
    val input = Iterator.continually(StdIn.readLine).takeWhile(_ ne null)
    `read-many-from-string`(input.mkString)
}

object DynamicPredef {
  object `print!` extends IInvokable:
    override def invoke(xs: Seq[Any]): Any =
      print(xs.mkString(" "))
      Nil

  object `println!` extends IInvokable:
    override def invoke(xs: Seq[Any]): Any =
      println(xs.mkString(" "))
      Nil

  object input extends IInvokable:
    override def invoke(xs: Seq[Any]): Any =
      `println!`.invoke(xs)
      StdIn.readLine()

  object `panic!` extends IInvokable:
    override def invoke(xs: Seq[Any]): Any =
      throw new RuntimeException(xs.mkString(" "))

  object `string` extends IInvokable:
    override def invoke(xs: Seq[Any]): Any =
      xs.mkString

  object int extends IDynamicInvokable:
    override def apply(arg: Any): Any = arg match
      case n: String => Predef.int(n)
      case n: Int => Predef.int(n)
      case n: Boolean => Predef.int(n)
}
